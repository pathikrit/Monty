import Card.Implicits._
import HandType._

object UnitTests extends App {

  def testEvaluation(myHand: Set[Card], expectedType: HandType.Value, sorting: Array[Card]) {
    val hand = new Hand(myHand)
    require (hand.handType == expectedType)
    require (hand.sorted.zip(sorting) forall {p => p._1.rank == p._2.rank})
  }

  testEvaluation ("qc kc tc ac jc", StraightFlush , "Ac kc qc jc tc")
  testEvaluation ("5c 4c 3c ac 2c", StraightFlush , "5c 4c 3c 2c ac")
  testEvaluation ("5c 4c 3c 6c 2c", StraightFlush , "6c 5c 4c 3c 2c")
  testEvaluation ("5d As 5c 5h 5s", FourOfAKind   , "5s 5c 5d 5h As")
  testEvaluation ("Td KH tc Th ks", FullHouse     , "Td Th Tc Kh Ks")
  testEvaluation ("qc kc 9c ac jc", Flush         , "Ac kc qc jc 9c")
  testEvaluation ("2c 5H 4S 3D ad", Straight      , "5h 4s 3d 2c ac")
  testEvaluation ("9c AH AS AD 8d", ThreeOfAKind  , "AH AS AD 9c 8d")
  testEvaluation ("ac 2s 2d 3s 3d", TwoPair       , "3s 3d 2s 2d ac")
  testEvaluation ("3d 3s ah td js", OnePair       , "3s 3d ah js td")
  testEvaluation ("2d 3s ah td js", HighCard      , "ah js td 3s 2d")
}
