package vending.domain

// Configuration used in Reader
case class Config(
  prices: Map[String, Int],
  validCoins: Set[Int],
  maxInserted: Int,
  validStudentIds: Set[String], // Set of valid student IDs for discounts
  discountPercent: Int          // Discount percentage
)
