package challenge

import org.scalatest._
import funsuite._
import org.scalatest.matchers.should.Matchers
import Problem2._

class Problem2Spec extends AnyFunSuite with Matchers {

  val inputPromptions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with
  )

  test("provided test case: all combinations") {
    val results = allCombinablePromotions(inputPromptions)
    results should contain theSameElementsAs Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )
  }

  test("no exclusions") {

    val inputPromptions = Seq(
      Promotion("P1", Seq.empty),
      Promotion("P2", Seq.empty),
      Promotion("P3", Seq.empty)
    )

    val results = allCombinablePromotions(inputPromptions)
    results should contain theSameElementsAs Seq(
      PromotionCombo(Seq("P1", "P2", "P3"))
    )
  }

  test("no promocodes") {
    val results = allCombinablePromotions(Seq.empty)
    results should equal(Nil)
  }

  test("one promocode is just itself") {
    val results = allCombinablePromotions(Seq(Promotion("P1", Seq("P3"))))
    results should contain theSameElementsAs Seq(
      PromotionCombo(Seq("P1"))
    )
  }

  test("two compatible items") {
    val results = allCombinablePromotions(
      Seq(Promotion("P1", Seq("P3")), Promotion("P2", Seq("P4", "P5")))
    )

    results should contain theSameElementsAs Seq(
      PromotionCombo(Seq("P1", "P2"))
    )
  }

  test("two incompatible items") {
    val results = allCombinablePromotions(
      Seq(Promotion("P1", Seq("P2")), Promotion("P2", Seq("P1", "P4", "P5")))
    )

    results should equal(Nil)
  }

  test("three incompatible items") {
    val results = allCombinablePromotions(
      Seq(
        Promotion("P1", Seq("P2", "P3")),
        Promotion("P2", Seq("P1", "P3")),
        Promotion("P3", Seq("P2", "P1"))
      )
    )

    results should equal(Nil)
  }

  test("provided test case: P1") {
    val results = combinablePromotions("P1", inputPromptions)
    results should contain theSameElementsAs Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )
  }

  test("provided test case: P3") {
    val results = combinablePromotions("P3", inputPromptions)
    results should contain theSameElementsAs Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )
  }

  test("combinablePromos: empty list") {
    val results = combinablePromotions("P3", Seq.empty)
    results should equal(Nil)
  }

  test("combinablePromos: no matching promo code") {
    val results = combinablePromotions("P6", inputPromptions)
    results should equal(Nil)
  }

}
