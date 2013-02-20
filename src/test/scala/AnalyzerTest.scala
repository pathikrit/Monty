object AnalyzerTest extends App {
  import Hand.Type._

  def cardsFromStr(s: String) = s split " " map Card.from

  def test(myHand: String, expectedType: Hand.Type.Value, sorting: String) {
    val hand = new Hand(cardsFromStr(myHand).toSet)
    require (hand.handType == expectedType)
    val expectedSort = cardsFromStr(sorting)
    require (hand.sorted.zip(expectedSort) forall {p => p._1.rank == p._2.rank})
  }

  test ("qc kc tc ac jc", StraightFlush , "Ac kc qc jc tc")
  test ("5c 4c 3c ac 2c", StraightFlush , "5c 4c 3c 2c ac")
  test ("5c 4c 3c 6c 2c", StraightFlush , "6c 5c 4c 3c 2c")
  test ("5d As 5c 5h 5s", FourOfAKind   , "5s 5c 5d 5h As")
  test ("Td KH tc Th ks", FullHouse     , "Td Th Tc Kh Ks")
  test ("qc kc 9c ac jc", Flush         , "Ac kc qc jc 9c")
  test ("2c 5H 4S 3D ad", Straight      , "5h 4s 3d 2c ac")
  test ("9c AH AS AD 8d", ThreeOfAKind  , "AH AS AD 9c 8d")
  test ("ac 2s 2d 3s 3d", TwoPair       , "3s 3d 2s 2d ac")
  test ("3d 3s ah td js", OnePair       , "3s 3d ah js td")
  test ("2d 3s ah td js", HighCard      , "ah js td 3s 2d")
}
