package challenge

object Problem2 extends App {
//   Background: Cruise bookings can have one or more Promotions applied to them. But sometimes a
// Promotion cannot be combined with another Promotion. Our application has to find out all possible
// Promotion Combinations that can be applied together.

  val inputPromptions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with
  )

// Implement a function to find all PromotionCombos with maximum number of combinable
// promotions in each. The function and case class definitions are supplied below to get you started.
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  private def generateValidCombos(
      exclusionMap: Map[String, Set[String]],
      distinctPromoCodes: Seq[String]
  ) = {
    // Generate them such that larger promos come first and later sequences might be a subset of
    // the larger ones for easier eliminations.
    val generatedCombos = (distinctPromoCodes.length to 2 by -1).flatMap(len =>
      distinctPromoCodes.combinations(len)
    )

    generatedCombos
      .foldLeft(List.empty[Set[String]]) {
        case (acc, nextCombo) => {
          // is valid combo
          val (_, valid) = nextCombo.foldLeft((Set.empty[String], true)) {
            case ((exclusions, true), next) =>
              if (!exclusions.contains(next)) {
                val exclusionsNext =
                  exclusionMap.getOrElse(next, Set.empty)
                (exclusions ++ exclusionsNext, true)
              } else (exclusions, false)
            case (o @ (exclusions, false), next) => o
          }

          val asSet = nextCombo.toSet

          if (valid && !acc.exists(set => asSet.subsetOf(set)))
            asSet :: acc
          else acc

        }
      }
      .filter(_.nonEmpty)
      .sortBy(_.headOption.getOrElse(""))
      .map(x => PromotionCombo(x.toList))
  }

  def allCombinablePromotions(
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val exclusionMap =
      allPromotions
        .map(x => x.code -> x.notCombinableWith.toSet)
        .toMap

    val distinctPromos = allPromotions.map(_.code).distinct

    generateValidCombos(exclusionMap, distinctPromos)
  }

  def combinablePromotions(
      promotionCode: String,
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val promo = allPromotions
      .find(_.code == promotionCode)

    promo match {
      case None => Seq.empty
      case Some(p) =>
        val filteredPromos = allPromotions
          .filter(x => !p.notCombinableWith.contains(x.code))

        val distinctPromos = filteredPromos
          .map(_.code)
          .distinct

        val exclusionMap = filteredPromos
          .map(x => x.code -> x.notCombinableWith.toSet)
          .toMap

        generateValidCombos(exclusionMap, distinctPromos)
    }
  }

  val results = allCombinablePromotions(inputPromptions)

  results.foreach(println)

  println()

  // expected output
//   Seq(
// PromotionCombo(Seq(P1, P2)),
// PromotionCombo(Seq(P1, P4, P5)),
// PromotionCombo(Seq(P2, P3)),
// PromotionCombo(Seq(P3, P4, P5))
// )

  val p1 = combinablePromotions("P1", inputPromptions)

  p1.foreach(println)

// Seq(
// PromotionCombo(Seq(P1, P2)),
// PromotionCombo(Seq(P1, P4, P5))
// )

}
