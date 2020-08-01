package retcalc


sealed abstract class RetCalcError(val message: String)
case class MoreExpensesThanIncome(income: Double, expenses: Double)
    extends RetCalcError(s"Expenses: $expenses >= $income!")
case class ReturnMonthOutOfBounds(month: Int, maximum: Int)
    extends RetCalcError(s"Cannot get return for month $month since out of range. Range is 0 to $maximum")