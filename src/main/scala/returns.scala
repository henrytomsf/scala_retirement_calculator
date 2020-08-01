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
case class OffsetReturns(orig: Returns, offset: Int) extends Returns



object Returns {
    def monthlyRate(returns: Returns, month: Int): Either[RetCalcError, Double] = returns match {
        case FixedReturns(r) => Right(r/12)
        case VariableReturns(rs) => if (rs.isDefinedAt(month)) {
            Right(rs(month).monthlyRate) //this monthlyRate is the attribute, not the static method
        } else {
            Left(ReturnMonthOutOfBounds(month, rs.size-1))
        }
        case OffsetReturns(rs, offset) => monthlyRate(rs, month+offset)
    }

    def fromEquityAndInflationData(equities: Vector[EquityData], inflations: Vector[InflationData]): VariableReturns = {
        VariableReturns(
            equities.zip(inflations)
            .sliding(2) //get previous item and current item together
            .collect {
                case (prevEquity, prevInflation) +: (equity, inflation) +: Vector() =>
                    val inflationRate = inflation.value / prevInflation.value
                    val totalReturn = (equity.value + equity.monthlyDividend) / prevEquity.value
                    val realTotalReturn = totalReturn - inflationRate
                    VariableReturn(equity.monthId, realTotalReturn)
            }
            .toVector
        )
    }
}