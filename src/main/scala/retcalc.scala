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
                      initialCapital: Double): Double = {
        val monthlySavings = netIncome - currentExpenses
        (0 until nbOfMonths).foldLeft(initialCapital){
            case (accumulated, month) =>
                accumulated*(1+Returns.monthlyRate(returns, month)) + monthlySavings
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
                                  nbOfMonthsInRetirement: Int,
                                  netIncome: Int,
                                  currentExpenses: Int,
                                  initialCapital: Double): Int = {
        @tailrec
        def loop(months: Int): Int = {
            val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
                returns = returns,
                nbOfMonthsSaving = months,
                nbOfMonthsInRetirement = nbOfMonthsInRetirement,
                netIncome = netIncome,
                currentExpenses = currentExpenses,
                initialCapital = initialCapital
            )
            if (capitalAfterDeath > 0.0) months else loop(months + 1)
        }
        if (netIncome > currentExpenses) {
            loop(0)
        } else {
            Int.MaxValue
        }
    }
}