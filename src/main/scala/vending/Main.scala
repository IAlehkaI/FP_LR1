package vending

import vending.monads.{IO, Writer, State}
import vending.domain.{Config, VendingMachineState, DomainLogic, VendingState}
import scala.util.Try

sealed trait MenuCommand
object MenuCommand {
  case object InsertCoin extends MenuCommand
  case object BuyProduct extends MenuCommand
  case object Refund extends MenuCommand
  case object RefillProduct extends MenuCommand
  case object NextDay extends MenuCommand
  case object ShowState extends MenuCommand
  case object ShowLog extends MenuCommand
  case object Exit extends MenuCommand
  case object Empty extends MenuCommand
  case class Unknown(raw: String) extends MenuCommand

  def parse(input: String): MenuCommand = input.trim match {
    case "1" => InsertCoin
    case "2" => BuyProduct
    case "3" => Refund
    case "4" => RefillProduct
    case "5" => NextDay
    case "6" => ShowState
    case "7" => ShowLog
    case "0" => Exit
    case ""  => Empty
    case other => Unknown(other)
  }
}

object Main {

  val config = Config(
    prices = Map("Cola" -> 100, "Juice" -> 120, "Water" -> 50, "Chips" -> 80),
    validCoins = Set(1, 2, 5, 10, 50, 100, 200),
    maxInserted = 1000,
    validStudentIds = Set("1111", "7777"),
    discountPercent = 20
  )

  val productList = config.prices.keys.toList.sorted

  val initialState = VendingMachineState(
    inventory = Map("Cola" -> 5, "Juice" -> 3, "Water" -> 10, "Chips" -> 2),
    insertedAmount = 0,
    revenue = 0,
    coinsTill = Map(1 -> 0, 2 -> 0, 5 -> 0, 10 -> 0, 50 -> 0, 100 -> 0, 200 -> 0),
    usedStudentIds = Set.empty,
    dayCounter = 1,
    currentSessionCoins = Nil // ИЗМЕНЕНИЕ: Инициализация пустого списка монет текущей сессии
  )

  val clearTerminal: IO[Unit] = IO.delay {
    print("\u001b[H\u001b[2J")
    System.out.flush()
  }

  def processCoin(coin: Int, currentWriter: Writer[Unit]): State[VendingMachineState, Writer[Unit]] = {
    val isAccepted = DomainLogic.canAcceptCoin(coin).run(config)
    val nextWriter = currentWriter.flatMap(_ => DomainLogic.explainInsert(coin, isAccepted))

    if (isAccepted) {
      VendingState.insertCoin(coin).map(_ => nextWriter)
    } else {
      State.pure(nextWriter)
    }
  }

  def processCoins(coins: List[Int], writer: Writer[Unit]): State[VendingMachineState, Writer[Unit]] = {
    coins match {
      case Nil => State.pure(writer)
      case head :: tail =>
        processCoin(head, writer).flatMap(nextWriter => processCoins(tail, nextWriter))
    }
  }

  def processPurchase(product: String, studentIdOpt: Option[String], currentWriter: Writer[Unit]): State[VendingMachineState, Writer[Unit]] = {
    val isStudent = studentIdOpt.isDefined
    val effectivePriceOpt = DomainLogic.effectivePrice(product, isStudent).run(config)

    effectivePriceOpt match {
      case Some(price) =>
        State.get[VendingMachineState].flatMap { s =>
          val writerWithAttempt = currentWriter.flatMap(_ => DomainLogic.explainPurchase(product, s.insertedAmount, price, isStudent))
          VendingState.selectProduct(product, price, studentIdOpt).map {
            case Right(change) =>
              writerWithAttempt.flatMap(_ => Writer.tell(s"Purchase successful! Change returned: $change. Enjoy your $product!"))
            case Left(err) =>
              writerWithAttempt.flatMap(_ => DomainLogic.explainFailure(err))
          }
        }
      case None =>
        State.pure(currentWriter.flatMap(_ => DomainLogic.explainFailure(s"Product '$product' not found in the machine.")))
    }
  }

  def renderMenu(state: VendingMachineState): IO[Unit] = {
    IO.delay {
      println("\n" + "="*45)
      println(f"      VENDING MACHINE (Day: ${state.dayCounter})")
      println("="*45)
      println(s" Balance: ${state.insertedAmount}")
      println("-" * 45)
      println(" Products available:")

      productList.zipWithIndex.foreach { case (p, idx) =>
        val price = DomainLogic.effectivePrice(p, isStudent = false).run(config).getOrElse(0)
        val studentPrice = DomainLogic.effectivePrice(p, isStudent = true).run(config).getOrElse(0)
        val count = state.inventory.getOrElse(p, 0)
        val status = if (count > 0) s"Qty: $count" else "OUT OF STOCK"
        println(f"  ${idx + 1}. $p%-10s : $price%3d coins ($studentPrice with student ID) [$status]")
      }

      println("-" * 45)
      println(" Actions:")
      println(s"  1 - Insert coin(s) (Valid: ${config.validCoins.toList.sorted.mkString(", ")})")
      println("  2 - Buy product")
      println("  3 - Refund / Cancel")
      println("  4 - Refill product (Admin)")
      println("  5 - Next Day (Reset student discounts)")
      println("  6 - Show internal State (Debug)")
      println("  7 - Show system Log history")
      println("  0 - Exit")
      println("="*45)
      print(" Select action: ")
    }
  }

  def askForStudentId(state: VendingMachineState): IO[Option[String]] = {
    for {
      _ <- IO.delay(print(" Do you have a student ID for a discount? (Enter ID or press Enter to skip): "))
      input <- IO.readLn()
      idStr = if (input != null) input.trim else ""
      result <- if (idStr.isEmpty) {
        IO.pure(None)
      } else if (!DomainLogic.isValidStudentId(idStr).run(config)) {
        IO.putStrLn(s"\n[!] Invalid student ID ($idStr). Proceeding without discount.").flatMap(_ => IO.pure(None))
      } else if (state.usedStudentIds.contains(idStr)) {
        IO.putStrLn(s"\n[!] Student ID ($idStr) has already been used today. Proceeding without discount.").flatMap(_ => IO.pure(None))
      } else {
        IO.pure(Some(idStr))
      }
    } yield result
  }

  def parseProductSelection(input: String): Option[String] = {
    val trimmed = input.trim
    Try(trimmed.toInt).toOption match {
      case Some(num) if num >= 1 && num <= productList.length =>
        Some(productList(num - 1))
      case _ =>
        productList.find(_.equalsIgnoreCase(trimmed))
    }
  }

  def loop(state: VendingMachineState, globalWriter: Writer[Unit]): IO[Unit] = {
    for {
      _ <- clearTerminal
      _ <- renderMenu(state)
      actionStr <- IO.readLn()
      _ <- if (actionStr == null) {
        IO.putStrLn("\n[!] Input stream closed. Exiting...").flatMap(_ => IO.pure(()))
      } else {
        val cmd = MenuCommand.parse(actionStr)
        handleCommand(cmd, state, globalWriter)
      }
    } yield ()
  }

  def handleCommand(cmd: MenuCommand, state: VendingMachineState, globalWriter: Writer[Unit]): IO[Unit] = {
    cmd match {
      case MenuCommand.InsertCoin =>
        for {
          _ <- IO.delay(print(s" Enter coin(s) separated by space (e.g. '10 50 10'): "))
          inputStr <- IO.readLn()
          safeInput = if (inputStr != null) inputStr.trim else ""
          coins = safeInput.split("\\s+").toList.flatMap(s => Try(s.toInt).toOption)

          _ <- if (coins.isEmpty) IO.putStrLn("\n[!] No valid coins entered.") else IO.pure(())

          (newState, nextWriter) = processCoins(coins, globalWriter).run(state)
          _ <- loop(newState, nextWriter)
        } yield ()

      case MenuCommand.BuyProduct =>
        for {
          _ <- IO.delay {
            println("\n Select product by Number (1-4) or Name (e.g. Cola):")
            print(" Selection: ")
          }
          productInput <- IO.readLn()
          safeInput = if (productInput != null) productInput.trim else ""

          _ <- parseProductSelection(safeInput) match {
            case Some(productName) =>
              for {
                studentIdOpt <- askForStudentId(state)
                (newState, nextWriter) = processPurchase(productName, studentIdOpt, globalWriter).run(state)
                _ <- loop(newState, nextWriter)
              } yield ()
            case None =>
              IO.putStrLn("\n[!] Invalid product selection.")
                .flatMap(_ => IO.delay(Thread.sleep(1000)))
                .flatMap(_ => loop(state, globalWriter))
          }
        } yield ()

      // ИЗМЕНЕНИЕ: Обработка возврата с удалением монет из кассы
      case MenuCommand.Refund =>
        val (newState, (refundAmount, coinsReturned)) = VendingState.cancelPurchase().run(state)
        val coinsStr = if (coinsReturned.isEmpty) "None" else coinsReturned.mkString(", ")
        val nextWriter = globalWriter.flatMap(_ =>
          Writer.tell(s"Refund issued: $refundAmount coins returned. Physically removed from till: [$coinsStr]")
        )
        loop(newState, nextWriter)

      case MenuCommand.RefillProduct =>
        for {
          _ <- IO.delay(print(" Enter product name to refill (e.g. Cola): "))
          pInput <- IO.readLn()
          _ <- IO.delay(print(" Enter amount to add: "))
          aInput <- IO.readLn()
          amount = Try(if (aInput != null) aInput.trim.toInt else 0).getOrElse(0)
          safeProduct = if (pInput != null) pInput.trim else ""

          _ <- parseProductSelection(safeProduct) match {
            case Some(productName) if amount > 0 =>
              val newState = VendingState.refillProduct(productName, amount).run(state)._1
              val nextWriter = globalWriter.flatMap(_ => Writer.tell(s"Admin refilled $amount units of $productName."))
              loop(newState, nextWriter)
            case _ =>
              IO.putStrLn("\n[!] Invalid product or amount.")
                .flatMap(_ => IO.delay(Thread.sleep(1000)))
                .flatMap(_ => loop(state, globalWriter))
          }
        } yield ()

      case MenuCommand.NextDay =>
        val (newState, newDay) = VendingState.nextDay().run(state)
        val nextWriter = globalWriter.flatMap(_ => Writer.tell(s"Advanced to Day $newDay. Student discounts reset."))
        loop(newState, nextWriter)

      case MenuCommand.ShowState =>
        IO.delay {
          println("\n" + "="*45)
          println(" CURRENT RAW STATE (Debug Info)")
          println("="*45)
          println(s" * Inventory:       ${state.inventory}")
          println(s" * Inserted Amount: ${state.insertedAmount}")
          println(s" * Total Revenue:   ${state.revenue}")

          val sortedTill = state.coinsTill.toList.sortBy(_._1).map { case (k, v) => s"$k -> $v" }.mkString(", ")
          println(s" * Coins in Till:   List($sortedTill)")
          println(s" * Used StudentIDs: ${state.usedStudentIds}")
          println(s" * Current Day:     ${state.dayCounter}")
          println(s" * Session Coins:   ${state.currentSessionCoins}") // Отображение монет текущей сессии
          println("="*45)
          print(" Press Enter to return to menu... ")
        }.flatMap(_ => IO.readLn()).flatMap(_ => loop(state, globalWriter))

      case MenuCommand.ShowLog =>
        IO.delay {
          println("\n" + "="*45)
          println(" SYSTEM LOG HISTORY")
          println("="*45)
          val logs = globalWriter.run._1
          if (logs.isEmpty) {
            println("  [Log is empty. No operations performed yet.]")
          } else {
            logs.foreach(l => println(s"  -> $l"))
          }
          println("="*45)
          print(" Press Enter to return to menu... ")
        }.flatMap(_ => IO.readLn()).flatMap(_ => loop(state, globalWriter))

      case MenuCommand.Exit =>
        IO.putStrLn("\nShutting down vending machine...")
          .flatMap(_ => IO.putStrLn("Final State Summary:"))
          .flatMap(_ => IO.putStrLn(s" Total Revenue: ${state.revenue} coins"))
          .flatMap(_ => IO.putStrLn(s" Remaining Inventory: ${state.inventory.filter(_._2 > 0).map{case (k,v) => s"$k($v)"}.mkString(", ")}"))
          .flatMap(_ => IO.putStrLn("Goodbye!"))

      case MenuCommand.Empty => loop(state, globalWriter)

      case MenuCommand.Unknown(raw) =>
        IO.putStrLn(s"\n[!] Unknown action '$raw'.")
          .flatMap(_ => IO.delay(Thread.sleep(1200)))
          .flatMap(_ => loop(state, globalWriter))
    }
  }

  def main(args: Array[String]): Unit = {
    loop(initialState, Writer.pure(())).unsafeRun()
  }
}