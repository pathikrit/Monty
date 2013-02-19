import scala.collection.concurrent.TrieMap
import scala.collection.mutable

case class Card(rank: Int, suit: Int) { override def toString = s"${(Card ranks rank)}${(Card suits suit)}" }

object Card {
  implicit val rankOrdering = Ordering by {card: Card => card.rank}
  val (ranks, suits) = ("23456789TJQKA", "♣♠♦♥")
  val all = for {rank <- 0 until ranks.length; suit <- 0 until suits.length} yield Card(rank, suit)
  def from(s: String) = Card(ranks indexOf s(0).toUpper, "CSDH" indexOf s(1).toUpper)
}

class Deck {
  val cards = mutable.Queue() ++ util.Random.shuffle(Card.all)
  def deal = cards dequeue
  def remove(discards: Set[Card]) { discards foreach {card: Card => cards dequeueFirst (_ == card)} }
  override def toString = cards mkString ","
}

class Hand(hand: Iterable[Card]) {
  val (handType, sorted) = {
    require(hand.size == 5)

    val (rankGroups, suitGroups) = (hand groupBy {_.rank}, hand groupBy {_.suit})
    val ranks = rankGroups.keySet

    def hasSameRanks(matches: Int = 2, required: Int = 1) = (rankGroups count {_._2.size == matches}) >= required

    val isFlush = suitGroups.size == 1
    val isWheel = "A2345" map {Card.ranks indexOf _} forall ranks.contains   // A,2,3,4,5 straight
    val isStraight = rankGroups.size == 5 && (ranks.max - ranks.min) == 4 || isWheel

    val handType = if (isStraight && isFlush)                           Hand.Type.StraightFlush
      else if (hasSameRanks(matches = 4))                               Hand.Type.FourOfAKind
      else if (hasSameRanks(matches = 3) && hasSameRanks(matches = 2))  Hand.Type.FullHouse
      else if (isFlush)                                                 Hand.Type.Flush
      else if (isStraight)                                              Hand.Type.Straight
      else if (hasSameRanks(matches = 3))                               Hand.Type.ThreeOfAKind
      else if (hasSameRanks(required = 2))                              Hand.Type.TwoPair
      else if (hasSameRanks())                                          Hand.Type.OnePair
      else                                                              Hand.Type.HighCard

    val tieBreakers = {
      def bestFromGrouping(g: Int) = (rankGroups filter (_._2.size == g)).values.flatten.toList.sorted
      val kickers = ((1 to 4) map bestFromGrouping).flatten.reverse.toSeq
      if (isWheel) kickers.takeRight(4) :+ kickers.head else kickers
    }
    (handType, tieBreakers)
  }
}

object Hand {
  import scala.math.Ordering.Implicits._

  object Type extends Enumeration {
    val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush = Value
  }

  implicit val ordering = Ordering by {hand: Hand => (hand.handType, hand.sorted)}

  def selectBest(hand: Set[Card]) = (for (c1 <- hand; c2 <- hand; if c1 != c2) yield new Hand(hand - (c1, c2))).max
}

class Stats {
  import Stats._

  var (expectedWin, expectedLoss) = (0.0, 0.0)
  val (wins, ties, losses) = (newTypeCounter, newTypeCounter, newTypeCounter)

  def logWin(handType: HT) { expectedWin += 1; wins(handType) += 1 }
  def logLoss(handType: HT) { expectedLoss += 1; losses(handType) += 1 }
  def logTie(handType: HT, split: Int) { expectedWin += 1.0/split; ties(handType) += 1 }

  def expectedReturn(potSize: Double, yourBets: Double) = potSize*expectedWin - yourBets*expectedLoss
}

object Stats {
  private type HT = Hand.Type.Value
  private def newTypeCounter = TrieMap[HT, Int]().withDefaultValue(0)

  def evaluate(myHand: Set[Card], board: Set[Card], otherPlayers: Int, simulations: Int = 1000) = {
    val stats = new Stats()
    (1 to simulations).par foreach (simulation => {
      val deck = new Deck()
      deck remove (myHand ++ board)
      val community = mutable.Set() ++ board
      val players = (1 to otherPlayers) map {i => Set(deck.deal, deck.deal)}
      while (community.size < 5) community += deck.deal

      val myBest = Hand.selectBest(myHand ++ community)
      val otherBests = players map {p => Hand.selectBest(p ++ community)}
      val matchUps = otherBests groupBy {p => Hand.ordering.compare(myBest, p).signum}

      myBest.handType match {
        case defeat if matchUps contains -1 => stats.logLoss(matchUps(-1).max.handType)
        case victory if !(matchUps contains 0) => stats.logWin(victory)
        case tied => stats.logTie(tied, matchUps(0).size + 1)
      }
    })
    stats
  }
}
