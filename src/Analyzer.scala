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
}

class Hand(hand: Set[Card]) {
  val (handType, sorted) = {
    require(hand.size == 5)

    val (rankGroups, suitGroups) = (hand groupBy {_.rank}, hand groupBy {_.suit})
    val ranks = rankGroups.keySet

    def hasSameRanks(matches: Int = 2, required: Int = 1) = (rankGroups count {_._2.size == matches}) >= required

    val isFlush = suitGroups.size == 1
    val isWheel = "A2345" map {Card.ranks indexOf _} forall ranks.contains   // A,2,3,4,5 straight
    val isStraight = rankGroups.size == 5 && (ranks.max - ranks.min) == 4 || isWheel
    val (isThreeOfAKind, isOnePair) = (hasSameRanks(matches = 3), hasSameRanks(matches = 2))

    val handType = if (isStraight && isFlush) Hand.Type.StraightFlush
      else if (hasSameRanks(matches = 4))     Hand.Type.FourOfAKind
      else if (isThreeOfAKind && isOnePair)   Hand.Type.FullHouse
      else if (isFlush)                       Hand.Type.Flush
      else if (isStraight)                    Hand.Type.Straight
      else if (isThreeOfAKind)                Hand.Type.ThreeOfAKind
      else if (hasSameRanks(required = 2))    Hand.Type.TwoPair
      else if (isOnePair)                     Hand.Type.OnePair
      else                                    Hand.Type.HighCard

    val tieBreakers = {
      def bestFromGrouping(g: Int) = (rankGroups filter (_._2.size == g)).values.flatten.toList.sorted
      val kickers = ((1 to 4) map bestFromGrouping).flatten.reverse.toSeq
      if (isWheel) (kickers takeRight 4) :+ kickers.head else kickers
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

  val bestCache = TrieMap[Set[Card], Hand]()
  def selectBest(hand: Set[Card]) = bestCache getOrElseUpdate (hand, generateAll(hand).max)
  private def generateAll(hand: Set[Card]) = for (c1 <- hand; c2 <- hand; if c1 != c2) yield new Hand(hand - (c1, c2))
}

case class Counter() {
  val count = TrieMap[Hand.Type.Value, Int]() withDefaultValue 0
  var total = 0
  def log(key: Hand.Type.Value) { count(key) += 1; total += 1 }
}

class Analysis {
  var (expectedWin, expectedLoss) = (0d, 0d)
  val (wins, ties, losses) = (Counter(), Counter(), Counter())
  def total = wins.total + ties.total + losses.total

  def reportWin(handType: Hand.Type.Value) { expectedWin += 1; wins log handType }
  def reportTie(handType: Hand.Type.Value, split: Int) { expectedWin += 1d/split; ties log handType}
  def reportLoss(handType: Hand.Type.Value) { expectedLoss += 1; losses log handType }

  def expectedReturn(canWin: Double, canLoss: Double) = (canWin*expectedWin - canLoss*expectedLoss)/total

  override def toString = {
    def percent(n: Int) = f"${100d * n /total}%5.2f%"
    def bucketDisplay(c: Counter) = (c.count map {e => f"${e._1}%15s: ${percent(e._2)}%s"}) mkString "\n"
    def totalDisplay(name: String, c: Counter) = s" $name: ${percent(c.total)}\n${bucketDisplay(c)}"
    s"${totalDisplay("Wins", wins)}\n${totalDisplay("Ties", ties)}\n${totalDisplay("Loss", losses)}"
  }
}

object Analyzer {
  def evaluate(myHand: Set[Card], board: Set[Card], otherPlayers: Int, simulations: Int = 10000) = {
    val analysis = new Analysis()
    for(simulation <- 1 to simulations) {
      val deck = new Deck()
      deck remove (myHand ++ board)
      val community = mutable.Set() ++ board
      val players = (1 to otherPlayers) map {i => Set(deck.deal, deck.deal)}
      while (community.size < 5) community += deck.deal

      val myBest = Hand selectBest (myHand ++ community)
      val otherBests = players map {p => Hand selectBest (p ++ community)}
      val matchUps = otherBests groupBy {p => Hand.ordering.compare(myBest, p).signum}

      myBest handType match {
        case defeat if matchUps contains -1 => analysis reportLoss matchUps(-1).max.handType
        case winningHand if !(matchUps contains 0) => analysis reportWin winningHand
        case tiedHand => analysis reportTie (tiedHand, matchUps(0).size + 1)
      }
    }
    require(analysis.total == simulations)
    analysis
  }
}
