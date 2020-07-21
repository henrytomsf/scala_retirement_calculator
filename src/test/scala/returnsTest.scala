package retcalc


import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec


class ReturnsSpec extends AnyFlatSpec {

    "VariableReturns.fromUntil" should "keep only a window of the return vector" in {
        val variableReturns = VariableReturns(Vector.tabulate(12)
            {i => val d = (i+1).toDouble
            VariableReturn(f"2017.$d%02.0f", d)}
        )
        variableReturns.fromUntil("2017.07", "2017.09").returns === (
            Vector(VariableReturn("2017.07", 7.0),
                    VariableReturn("2017.08", 8.0))
        )

        variableReturns.fromUntil("2017.10", "2018.01").returns === (
            Vector(VariableReturn("2017.10", 10.0),
                    VariableReturn("2017.11", 11.0),
                    VariableReturn("2017.12", 12.0))
        )
    }

    "Returns.monthlyRate" should "return a fixed rate for a FixedReturn object" in {
        Returns.monthlyRate(FixedReturns(0.04), 0) === (0.04/12)
        Returns.monthlyRate(FixedReturns(0.04), 10) === (0.04/12)
    }

    val variableReturns = VariableReturns(Vector(
        VariableReturn("2000.01", 0.1),
        VariableReturn("2000.02", 0.2)))
    it should "return the nth rate for VariableReturn" in {
        Returns.monthlyRate(variableReturns, 0) === (0.1)
        Returns.monthlyRate(variableReturns, 1) === (0.2)
    }

    it should "roll over from the first rate if n > length" in {
        Returns.monthlyRate(variableReturns, 2) === (0.1)
        Returns.monthlyRate(variableReturns, 3) === (0.2)
        Returns.monthlyRate(variableReturns, 4) === (0.1)
    }

    "Returns.monthlyReturn" should "return a fixed rate for a FixedReturn" in {
    }

    it should "return the nth rate for VariableReturn" in {
    }

    it should "return an error if n > length" in {
    }

    it should "return the n+offset-th rate for OffsetReturn" in {
        val returns = OffsetReturns(variableReturns, 1)
        Returns.monthlyRate(returns, 0) === (0.2)
    }

}