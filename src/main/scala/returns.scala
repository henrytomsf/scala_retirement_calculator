package retcalc


sealed trait Returns
case class FixedReturns(annualRate: Double) extends Returns
case class VariableReturn(monthId: String, monthlyRate: Double)
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
    def fromUntil(monthIdFrom: String, monthIdUntil: String): VariableReturns = {
        VariableReturns(
            returns.dropWhile(_.monthId != monthIdFrom)
                   .takeWhile(_.monthId != monthIdUntil)
        )
    }
}



object Returns {
    def monthlyRate(returns: Returns, month: Int): Double = returns match {
        case FixedReturns(r) => r/12
        case VariableReturns(rs) => rs(month % rs.length).monthlyRate //this monthlyRate is the attribute, not the static method
    }
}