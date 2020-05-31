package retcalc


sealed trait Returns
case class FixedReturns(annualRate: Double) extends Returns
case class VariableReturn(monthId: String, monthlyRate: Double)
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns
