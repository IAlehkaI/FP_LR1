package vending.domain

import vending.monads.{Reader, Writer}

object DomainLogic {

  // -- READER --

  def priceOf(product: String): Reader[Config, Option[Int]] = Reader { config =>
    config.prices.get(product)
  }

  def canAcceptCoin(coin: Int): Reader[Config, Boolean] = Reader { config =>
    config.validCoins.contains(coin)
  }

  // Calculates the price considering the student discount
  def effectivePrice(product: String, isStudent: Boolean): Reader[Config, Option[Int]] = Reader { config =>
    config.prices.get(product).map { price =>
      if (isStudent) {
        (price * (100 - config.discountPercent) / 100.0).toInt
      } else {
        price
      }
    }
  }

  // Check if student ID is valid
  def isValidStudentId(id: String): Reader[Config, Boolean] = Reader { config =>
    config.validStudentIds.contains(id)
  }

  def calculateChange(inserted: Int, price: Int): Int = inserted - price

  // -- WRITER --

  def explainInsert(coin: Int, accepted: Boolean): Writer[Unit] = {
    if (accepted) Writer.tell(s"Coin $coin accepted.")
    else Writer.tell(s"Coin $coin is not a valid denomination and was returned.")
  }

  def explainPurchase(product: String, inserted: Int, price: Int, isStudent: Boolean): Writer[Unit] = {
    val discountMsg = if (isStudent) " (with student discount)" else ""
    Writer.tell(s"Attempting to purchase $product$discountMsg for $price. Inserted: $inserted.")
  }

  def explainFailure(reason: String): Writer[Unit] = {
    Writer.tell(s"Purchase failed: $reason")
  }
}
