package retcalc


sealed abstract class RetCalcError(val message: String)


object RetCalcError {
    case class MoreExpensesThanIncome(income: Double, expenses: Double)
        extends RetCalcError(s"Expenses: $expenses >= $income!")
}