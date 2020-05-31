package retcalc

object RetCalc {
    def futureCapital(interestRate: Double,
                      nbOfMonths: Int,
                      netIncome: Int,
                      currentExpenses: Int,
                      initialCapital: Double): Double = {
        val monthlySavings = netIncome - currentExpenses
        def nextCapital(accumulated: Double, month: Int): Double = {
            accumulated*(1+interestRate) + monthlySavings
        }
        (0 until nbOfMonths).foldLeft(initialCapital)(nextCapital)
    }

    def simulatePlan(interestRate: Double,
                     nbOfMonthsSaving: Int,
                     nbOfMonthsInRetirement: Int,
                     netIncome: Int,
                     currentExpenses: Int,
                     initialCapital: Double): (Double, Double) = {
        val capitalAtRetirement = futureCapital(
            interestRate = interestRate,
            nbOfMonths = nbOfMonthsSaving,
            netIncome = netIncome,
            currentExpenses = currentExpenses,
            initialCapital = initialCapital
        )

        val capitalAfterDeath = futureCapital(
            interestRate = interestRate,
            nbOfMonths = nbOfMonthsInRetirement,
            netIncome = netIncome,
            currentExpenses = currentExpenses,
            initialCapital = initialCapital
        )
        (capitalAtRetirement, capitalAfterDeath)
    }
}