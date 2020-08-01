package retcalc


import scala.annotation.tailrec


case class RetCalcParams(nbOfMonthsInRetirement: Int,
                         netIncome: Int,
                         currentExpenses: Int,
                         initialCapital: Double)


object RetCalc {
    def futureCapital(returns: Returns,
                      nbOfMonths: Int,
                      netIncome: Int,
                      currentExpenses: Int,
                      initialCapital: Double): Either[RetCalcError, Double] = {
        val monthlySavings = netIncome - currentExpenses
        (0 until nbOfMonths).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)) { //need Right() here since folding an Either
            case (accumulated, month) =>
                for {
                    acc <- accumulated
                    monthlyRate <- Returns.monthlyRate(returns, month)
                } yield acc*(1+monthlyRate) + monthlySavings
        }
    }

    def simulatePlan(returns: Returns,
                     params: RetCalcParams,
                     nbOfMonthsSaving: Int): (Double, Double) = {
        import params._ //imports all attributes of RetCalcParams into scope to use
        // can also use params.netIncome, etc.
        val capitalAtRetirement = futureCapital(
            returns = returns,
            nbOfMonths = nbOfMonthsSaving,
            netIncome = netIncome,
            currentExpenses = currentExpenses,
            initialCapital = initialCapital
        )

        val capitalAfterDeath = futureCapital(
            returns = OffsetReturns(returns, nbOfMonthsSaving),
            nbOfMonths = nbOfMonthsInRetirement,
            netIncome = 0,
            currentExpenses = currentExpenses,
            initialCapital = capitalAtRetirement
        )
        (capitalAtRetirement, capitalAfterDeath)
    }

    def determinenbOfMonthsSaving(returns: Returns,
                                  params: RetCalcParams): Either[RetCalcError, Int] = {
        import params._
        @tailrec
        def loop(months: Int): Int = {
            val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
                returns = returns,
                params = params,
                nbOfMonthsSaving = months
            )
            if (capitalAfterDeath > 0.0) months else loop(months + 1)
        }
        if (netIncome > currentExpenses) {
            Right(loop(0))
        } else {
            Left(MoreExpensesThanIncome(netIncome, currentExpenses))
        }
    }
}