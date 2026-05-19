package vending.domain

import vending.monads.State

// The state of the vending machine
case class VendingMachineState(
                                inventory: Map[String, Int],        // Stock of products
                                insertedAmount: Int,                // Currently inserted money
                                revenue: Int,                       // Total collected revenue
                                coinsTill: Map[Int, Int],           // Cash register breakdown
                                usedStudentIds: Set[String],        // Student IDs that have already used their discount today
                                dayCounter: Int                     // Tracks the current day to reset discounts
                              )

object VendingState {

  def insertCoin(coin: Int): State[VendingMachineState, Unit] = State.modify { s =>
    s.copy(
      insertedAmount = s.insertedAmount + coin,
      coinsTill = s.coinsTill.updated(coin, s.coinsTill.getOrElse(coin, 0) + 1)
    )
  }

  def selectProduct(product: String, price: Int, studentIdOpt: Option[String]): State[VendingMachineState, Either[String, Int]] = State { s =>
    if (s.insertedAmount < price) {
      (s, Left(s"Insufficient funds. Inserted: ${s.insertedAmount}, Price: $price."))
    } else if (s.inventory.getOrElse(product, 0) <= 0) {
      (s, Left(s"Product $product is out of stock."))
    } else {
      val change = s.insertedAmount - price

      // If a student ID was used, add it to the used set
      val newUsedIds = studentIdOpt match {
        case Some(id) => s.usedStudentIds + id
        case None => s.usedStudentIds
      }

      val newState = s.copy(
        inventory = s.inventory.updated(product, s.inventory(product) - 1),
        insertedAmount = 0,
        revenue = s.revenue + price,
        usedStudentIds = newUsedIds
      )
      (newState, Right(change))
    }
  }

  def cancelPurchase(): State[VendingMachineState, Int] = State { s =>
    val refund = s.insertedAmount
    (s.copy(insertedAmount = 0), refund)
  }

  // ДОБАВЛЕНО: функция пополнения товаров (refillProduct) согласно требованиям
  def refillProduct(product: String, amount: Int): State[VendingMachineState, Unit] = State.modify { s =>
    val currentStock = s.inventory.getOrElse(product, 0)
    s.copy(
      inventory = s.inventory.updated(product, currentStock + amount)
    )
  }

  def nextDay(): State[VendingMachineState, Int] = State { s =>
    val next = s.dayCounter + 1
    // Clear used student IDs for the new day
    (s.copy(dayCounter = next, usedStudentIds = Set.empty), next)
  }
}