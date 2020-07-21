package retcalc


import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest._


class RetCalcSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {
    implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.0001)

    "RetCalc.futureCapital" should {
        "calculate the amount of savings I will have in n months" in {
            val actual = RetCalc.futureCapital(
                returns = FixedReturns(0.04),
                nbOfMonths = 25*12,
                netIncome = 3000,
                currentExpenses = 2000,
                initialCapital = 10000)
            val expected = 541267.1990
            actual should === (expected)
        }
    }

    "RetCalc.futureCapital" should {
        "calculate how much savings will be left after having taken a pension for n months" in {
            val actual = RetCalc.futureCapital(
                returns = FixedReturns(0.04),
                nbOfMonths = 40*12,
                netIncome = 0,
                currentExpenses = 2000,
                initialCapital = 541267.1990)
            val expected = 309867.53176
            actual should === (expected)
        }
    }

    val params = RetCalcParams(
        nbOfMonthsInRetirement = 40*12,
        netIncome = 3000,
        currentExpenses = 2000,
        initialCapital = 10000
    )

    "RetCalc.simulatePlan" should {
        "calculate the capital at retirement and the capital after death" in {
            val (capitalAfterRetirement, capitalAfterDeath) = RetCalc.simulatePlan(
                returns = FixedReturns(0.04),
                params,
                nbOfMonthsSaving = 25*12
            )
            capitalAfterRetirement should === (541267.1990)
            capitalAfterDeath should === (309867.5316)
        }

        "use different returns for capitalization and drawdown" in {
            val nbOfMonthsSaving = 25*12
            val returns = VariableReturns(
                Vector.tabulate(nbOfMonthsSaving + params.nbOfMonthsInRetirement)(i => 
                    if (i < nbOfMonthsSaving)
                        VariableReturn(i.toString, 0.04/12)
                    else
                        VariableReturn(i.toString, 0.03/12)
                )
            )
            val (capitalAfterRetirement, capitalAfterDeath) = RetCalc.simulatePlan(
                returns,
                params,
                nbOfMonthsSaving
            )
            capitalAfterRetirement should === (541267.1990)
            capitalAfterDeath should === (-57737.7227)
        }
    }

    "RetCalc.determinenbOfMonthsSaving" should {
        "calculate how long I need to save before I can retire" in {
            val actual = RetCalc.determinenbOfMonthsSaving(
                returns = FixedReturns(0.04),
                nbOfMonthsInRetirement = 40*12,
                netIncome = 3000,
                currentExpenses = 2000,
                initialCapital = 10000
            )
            val expected = 23 * 12 + 1
            actual should === (expected)
        }
        "not crash if the resulting nbOfMonths is very high" in {
            val actual = RetCalc.determinenbOfMonthsSaving(
                returns = FixedReturns(0.01),
                nbOfMonthsInRetirement = 40*12,
                netIncome = 3000,
                currentExpenses = 2999,
                initialCapital = 0
            )
            val expected = 8280
            actual should === (expected)
        }
        "not loop forever if bad parameters are passed" in {
            val actual = RetCalc.determinenbOfMonthsSaving(
                returns = FixedReturns(0.04),
                nbOfMonthsInRetirement = 40*12,
                netIncome = 1000,
                currentExpenses = 2000,
                initialCapital = 10000
            )
            actual should === (Int.MaxValue)
        }
    }
}