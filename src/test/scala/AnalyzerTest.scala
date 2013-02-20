object AnalyzerTest extends App {
  import Hand.Type._

  def cardsFromStr(s: String) = s split " " map Card.from

  def test(myHand: String, expectedType: Hand.Type.Value, sorting: String) {
    val hand = new Hand(cardsFromStr(myHand).toSet)
    require (hand.handType == expectedType)
    val expectedSort = cardsFromStr(sorting)
    //require (hand.sorted.zip(expectedSort) forall())//  {_._1.rank == _._2.rank})
  }

  test ("9c AH AS AD 8d", ThreeOfAKind, "AH AS AD 9c 8d")
}
