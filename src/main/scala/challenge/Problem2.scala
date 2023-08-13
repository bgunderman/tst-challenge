package challenge

object Problem2 extends App {
//   Background: Cruise bookings can have one or more Promotions applied to them. But sometimes a
// Promotion cannot be combined with another Promotion. Our application has to find out all possible
// Promotion Combinations that can be applied together.

// Implement a function to find all PromotionCombos with maximum number of combinable
// promotions in each.
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  private def generateValidCombos(
      exclusionMap: Map[String, Set[String]],
      distinctPromoCodes: Seq[String]
  ): Seq[PromotionCombo] = distinctPromoCodes match {
    case Nil                  => Nil
    case org @ (first +: Nil) => Seq(PromotionCombo(org))
    case org @ (first +: second +: Nil) =>
      exclusionMap.get(first) match {
        case Some(exclusions) if exclusions.contains(second) =>
          Nil
        case _ => Seq(PromotionCombo(org))
      }
    case distinctPromoCodes =>
      // Generate them such that larger promos come first and later sequences might be a subset of
      // the larger ones for easier eliminations.
      val generatedCombos =
        (distinctPromoCodes.length to 2 by -1).flatMap(len =>
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

  // finds all PromotionCombos with maximum number of combinable
  // promotions in each. Assumption min of 2 promocodes;
  // otherwise returns empty.
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

  // Find combinations with a specific promto code.
  // Assumption min of 2 promocodes;
  // otherwise returns empty.
  def combinablePromotions(
      promotionCode: String,
      allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val promo = allPromotions
      .find(_.code == promotionCode)

    promo match {
      case None => Nil
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

}
