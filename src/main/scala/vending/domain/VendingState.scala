package vending.domain

import vending.monads.State

case class VendingMachineState(
                                inventory: Map[String, Int],        // Stock of products
                                insertedAmount: Int,                // Currently inserted money
                                revenue: Int,                       // Total collected revenue
                                coinsTill: Map[Int, Int],           // Cash register breakdown
                                usedStudentIds: Set[String],        // Student IDs that have already used their discount today
                                dayCounter: Int,                    // Tracks the current day to reset discounts
                                currentSessionCoins: List[Int]      // ИЗМЕНЕНИЕ: Монеты, внесенные в рамках текущей сессии (для возврата)
                              )

object VendingState {

  def insertCoin(coin: Int): State[VendingMachineState, Unit] = State.modify { s =>
    s.copy(
      insertedAmount = s.insertedAmount + coin,
      coinsTill = s.coinsTill.updated(coin, s.coinsTill.getOrElse(coin, 0) + 1),
      currentSessionCoins = coin :: s.currentSessionCoins // Запоминаем внесенную монету
    )
  }

  def selectProduct(product: String, price: Int, studentIdOpt: Option[String]): State[VendingMachineState, Either[String, Int]] = State { s =>
    if (s.insertedAmount < price) {
      (s, Left(s"Insufficient funds. Inserted: ${s.insertedAmount}, Price: $price."))
    } else if (s.inventory.getOrElse(product, 0) <= 0) {
      (s, Left(s"Product $product is out of stock."))
    } else {
      val change = s.insertedAmount - price

      val newUsedIds = studentIdOpt match {
        case Some(id) => s.usedStudentIds + id
        case None => s.usedStudentIds
      }

      val newState = s.copy(
        inventory = s.inventory.updated(product, s.inventory(product) - 1),
        insertedAmount = 0,
        revenue = s.revenue + price,
        usedStudentIds = newUsedIds,
        currentSessionCoins = Nil // Сессия успешно завершена, очищаем список внесенных монет
      )
      (newState, Right(change))
    }
  }

  // удаление из кассы (coinsTill) при возврате
  def cancelPurchase(): State[VendingMachineState, (Int, List[Int])] = State { s =>
    val refundAmount = s.insertedAmount
    val coinsToReturn = s.currentSessionCoins

    // Рекурсивно вычитаем каждую возвращаемую монету из кассы автомата
    def removeCoins(till: Map[Int, Int], coins: List[Int]): Map[Int, Int] = coins match {
      case Nil => till
      case coin :: tail =>
        val currentCount = till.getOrElse(coin, 0)
        val newCount = if (currentCount > 0) currentCount - 1 else 0
        removeCoins(till.updated(coin, newCount), tail)
    }

    val updatedTill = removeCoins(s.coinsTill, coinsToReturn)

    val newState = s.copy(
      insertedAmount = 0,
      coinsTill = updatedTill,
      currentSessionCoins = Nil // Очищаем текущую сессию
    )

    (newState, (refundAmount, coinsToReturn))
  }

  def refillProduct(product: String, amount: Int): State[VendingMachineState, Unit] = State.modify { s =>
    val currentStock = s.inventory.getOrElse(product, 0)
    s.copy(
      inventory = s.inventory.updated(product, currentStock + amount)
    )
  }

  def nextDay(): State[VendingMachineState, Int] = State { s =>
    val next = s.dayCounter + 1
    (s.copy(dayCounter = next, usedStudentIds = Set.empty), next)
  }
}