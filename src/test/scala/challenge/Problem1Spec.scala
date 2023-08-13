package challenge

import org.scalatest._
import funsuite._
import org.scalatest.matchers.should.Matchers
import Problem1._

class Problem1Spec extends AnyFunSuite with Matchers {

  test("Provided test cases") {
    val inputRates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val inputPrices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val res = getBestGroupPrices(inputRates, inputPrices)
    res should contain theSameElementsAs (Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    ))

  }

  test("Only one input rate") {
    val inputRates = Seq(
      Rate("M1", "Military")
    )

    val inputPrices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val res = getBestGroupPrices(inputRates, inputPrices)
    res should contain theSameElementsAs (Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CB", "M1", 230.00, "Military")
    ))

  }

  test("No matching input prices to rates") {
    val inputRates = Seq(
      Rate("M1", "Military")
    )

    val inputPrices = Seq(
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val res = getBestGroupPrices(inputRates, inputPrices)
    res.isEmpty should equal(true)
  }

  test("No rates no results") {
    val inputRates = Seq(
    )

    val inputPrices = Seq(
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val res = getBestGroupPrices(inputRates, inputPrices)
    res.isEmpty should equal(true)
  }

  test("No rates no inputs no results") {
    val inputRates = Seq(
    )

    val inputPrices = Seq(
    )

    val res = getBestGroupPrices(inputRates, inputPrices)
    res.isEmpty should equal(true)
  }

}
