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
                     nbOfMonthsSaving: Int): Either[RetCalcError, (Double, Double)] = {
        import params._ //imports all attributes of RetCalcParams into scope to use
        // can also use params.netIncome, etc.

        for {
            capitalAtRetirement <- futureCapital(
                returns = returns,
                nbOfMonths = nbOfMonthsSaving,
                netIncome = netIncome,
                currentExpenses = currentExpenses,
                initialCapital = initialCapital
            )
            capitalAfterDeath <- futureCapital(
                returns = OffsetReturns(returns, nbOfMonthsSaving),
                nbOfMonths = nbOfMonthsInRetirement,
                netIncome = 0,
                currentExpenses = currentExpenses,
                initialCapital = capitalAtRetirement 
                                    //^ in for comprehension, this will get the value out of the option so it
                                    //  can be passed into the next function in the for comprehension
                                    /*
                                        val q = Right[String, Int](1)
                                        q.flatMap(foo => Right[String,Int](100))
                                                foo has been extracted from q as Int
                                                ex. val x = List(List(Right[String, Int](10), Right[String, Int](20)))
                                                    x.flatMap(item => item)
                                                    res: List[scala.util.Right[String,Int]] = List(Right(10), Right(20))
                                        res: scala.util.Either[String,Int] = Right(100)
                                    */
            )
        } yield (capitalAtRetirement, capitalAfterDeath)
    }

    def determinenbOfMonthsSaving(returns: Returns,
                                  params: RetCalcParams): Either[RetCalcError, Int] = {
        import params._
        @tailrec
        def loop(months: Int): Either[RetCalcError, Int] = {
            simulatePlan(returns, params, months) match {
                case Right((capitalAtRetirement, capitalAfterDeath)) => 
                    if (capitalAtRetirement > 0.0)
                        Right(months)
                    else
                        loop(months + 1)
                case Left(err) => Left(err)
            }
        }
        if (netIncome > currentExpenses) {
            loop(0)
        } else {
            Left(MoreExpensesThanIncome(netIncome, currentExpenses))
        }
    }
}