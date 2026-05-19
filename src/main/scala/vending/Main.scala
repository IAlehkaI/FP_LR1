package vending

import vending.monads.{IO, Writer, State}
import vending.domain.{Config, VendingMachineState, DomainLogic, VendingState}
import scala.util.Try

object Main {

  val config = Config(
    prices = Map("Cola" -> 100, "Juice" -> 120, "Water" -> 50, "Chips" -> 80),
    validCoins = Set(1, 2, 5, 10, 50, 100, 200),
    maxInserted = 1000,
    validStudentIds = Set("1111", "7777"),
    discountPercent = 20
  )

  // Sorted list of products to keep consistent numbering
  val productList = config.prices.keys.toList.sorted

  val initialState = VendingMachineState(
    inventory = Map("Cola" -> 5, "Juice" -> 3, "Water" -> 10, "Chips" -> 2),
    insertedAmount = 0,
    revenue = 0,
    coinsTill = Map(1 -> 0, 2 -> 0, 5 -> 0, 10 -> 100, 50 -> 20, 100 -> 10, 200 -> 0),
    usedStudentIds = Set.empty,
    dayCounter = 1
  )

  def processCoin(coin: Int, currentWriter: Writer[Unit]): State[VendingMachineState, Writer[Unit]] = {
    val isAccepted = DomainLogic.canAcceptCoin(coin).run(config)
    val nextWriter = currentWriter.flatMap(_ => DomainLogic.explainInsert(coin, isAccepted))

    if (isAccepted) {
      VendingState.insertCoin(coin).map(_ => nextWriter)
    } else {
      State.pure(nextWriter)
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
      println(s"  1 - Insert coin (Valid: ${config.validCoins.toList.sorted.mkString(", ")})")
      println("  2 - Buy product")
      println("  3 - Refund / Cancel")
      println("  4 - Next Day (Reset student discounts)")
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
    // Check if input is a number
    Try(trimmed.toInt).toOption match {
      case Some(num) if num >= 1 && num <= productList.length =>
        Some(productList(num - 1))
      case _ =>
        // Check if input matches product name (case-insensitive)
        productList.find(_.equalsIgnoreCase(trimmed))
    }
  }

  def loop(state: VendingMachineState): IO[Unit] = {
    for {
      _ <- renderMenu(state)
      actionStr <- IO.readLn()
      _ <- if (actionStr == null) {
             IO.putStrLn("\n[!] Input stream closed. Exiting...").flatMap(_ => IO.pure(()))
           } else {
             handleAction(actionStr.trim, state)
           }
    } yield ()
  }

  def handleAction(action: String, state: VendingMachineState): IO[Unit] = {
    action match {
      case "1" =>
        for {
          _ <- IO.delay(print(s" Enter coin value (${config.validCoins.toList.sorted.mkString("/")}): "))
          coinStr <- IO.readLn()
          coin = if (coinStr != null) Try(coinStr.trim.toInt).getOrElse(-1) else -1
          _ <- if (coin == -1) IO.putStrLn("\n[!] Invalid number format.")
               else IO.pure(())

          (newState, writer) = processCoin(coin, Writer.pure(())).run(state)
          _ <- IO.delay {
                 if (coin != -1) {
                   println("\n[Logs]:")
                   writer.run._1.foreach(l => println(s" -> $l"))
                 }
               }
          _ <- loop(newState)
        } yield ()

      case "2" =>
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
                     (newState, writer) = processPurchase(productName, studentIdOpt, Writer.pure(())).run(state)
                     _ <- IO.delay {
                            println("\n[Transaction Logs]:")
                            writer.run._1.foreach(l => println(s" -> $l"))
                          }
                     _ <- loop(newState)
                   } yield ()
                 case None =>
                   IO.putStrLn("\n[!] Invalid product selection.").flatMap(_ => loop(state))
               }
        } yield ()

      case "3" =>
        val (newState, refund) = VendingState.cancelPurchase().run(state)
        IO.putStrLn(s"\n[!] Refund issued: $refund coins.").flatMap(_ => loop(newState))

      case "4" =>
        val (newState, newDay) = VendingState.nextDay().run(state)
        IO.putStrLn(s"\n[!] Advanced to Day $newDay. All student discounts have been reset.")
          .flatMap(_ => loop(newState))

      case "0" =>
        IO.putStrLn("\nShutting down vending machine...")
          .flatMap(_ => IO.putStrLn("Final State Summary:"))
          .flatMap(_ => IO.putStrLn(s" Total Revenue: ${state.revenue} coins"))
          .flatMap(_ => IO.putStrLn(s" Remaining Inventory: ${state.inventory.filter(_._2 > 0).map{case (k,v) => s"$k($v)"}.mkString(", ")}"))
          .flatMap(_ => IO.putStrLn("Goodbye!"))

      case "" => loop(state) // Empty enter

      case _ =>
        IO.putStrLn("\n[!] Unknown action. Please select a valid number.").flatMap(_ => loop(state))
    }
  }

  def main(args: Array[String]): Unit = {
    loop(initialState).unsafeRun()
  }
}
