package challenge

object Problem1 extends App {

//  Background: The TST cruise application receives pricing and rate information from a third party data
// provider. We make two calls to this provider to receive a list of rates and a list of cabin prices. We can use
// this data to solve several problems for our customers. The problem we’ll be focusing on for this exercise
// will be finding the best price for a particular rate group.
// Cabin Price: The price for a specific cabin on a specific cruise. All cabin prices will have a single rate
// attached.

// Rate: A rate is a way to group related prices together. A rate is defined by its Rate Code and which Rate
// Group it belongs to. For example. (MilAB, Military) and (Sen123, Senior)
// Rate Group: Specific rates are grouped into a related rate group. There is a one-to-many relationship
// between rate groups and rates (A rate group is made up of many rates, but a rate can only belong to a
// single rate group) Some examples of rate groups are: Standard, Military, Senior, and Promotion.

  def getBestGroupPrices(
      rates: Seq[Rate],
      prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    val rateMap = rates.map(x => x.rateCode -> x.rateGroup).toMap

    prices
      .filter(price => rateMap.contains(price.rateCode))
      .groupBy(x => rateMap(x.rateCode) -> x.cabinCode)
      .flatMap { case ((group, cabinCode), values) =>
        values
          .sortBy(_.price)
          .headOption
          .map { cabin =>
            BestGroupPrice(cabin.cabinCode, cabin.rateCode, cabin.price, group)
          }
      }
      .toSeq
      .sortBy(_.price)

  }

  case class Rate(rateCode: String, rateGroup: String)
  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
  case class BestGroupPrice(
      cabinCode: String,
      rateCode: String,
      price: BigDecimal,
      rateGroup: String
  )
}
