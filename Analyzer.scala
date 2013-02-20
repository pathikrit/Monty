import scala.collection.concurrent.TrieMap
import scala.collection.mutable

case class Card(rank: Int, suit: Int) { override def toString = s"${(Card ranks rank)}${(Card suits suit)}" }

object Card {
  implicit val rankOrdering = Ordering by {card: Card => card.rank}
  val (ranks, suits) = ("23456789TJQKA", "♣♠♦♥")
  val all = for {rank <- 0 until ranks.length; suit <- 0 until suits.length} yield Card(rank, suit)

  object Implicits {
    implicit def fromStr(s: String) = Card(ranks indexOf s(0).toUpper, "CSDH" indexOf s(1).toUpper)
    implicit def fromLine(line: String) = if (line.trim.isEmpty) Array.empty[Card] else line split " " map fromStr
    implicit def toCardSet(line: String) = fromLine(line).toSet
  }
}

class Deck {
  val cards = mutable.Queue() ++ util.Random.shuffle(Card.all)
  def deal = cards dequeue
  def remove(discards: Set[Card]) { discards foreach {card: Card => cards dequeueFirst (_ == card)} }
}

object HandType extends Enumeration {
  val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush = Value
}

class Hand(hand: Iterable[Card]) {
  val (handType, sorted) = {
    def rankMatches(card: Card) = hand count (_.rank == card.rank)
    val groups = hand groupBy rankMatches mapValues {_.toList.sorted}

    val isFlush = (hand groupBy {_.suit}).size == 1
    val isWheel = "A2345" forall {r => hand exists (_.rank == Card.ranks.indexOf(r))}   // A,2,3,4,5 straight
    val isStraight = groups.size == 1 && (hand.max.rank - hand.min.rank) == 4 || isWheel
    val (isThreeOfAKind, isOnePair) = (groups contains 3, groups contains 2)

    val handType = if (isStraight && isFlush)     HandType.StraightFlush
      else if (groups contains 4)                 HandType.FourOfAKind
      else if (isThreeOfAKind && isOnePair)       HandType.FullHouse
      else if (isFlush)                           HandType.Flush
      else if (isStraight)                        HandType.Straight
      else if (isThreeOfAKind)                    HandType.ThreeOfAKind
      else if (isOnePair && groups(2).size == 4)  HandType.TwoPair
      else if (isOnePair)                         HandType.OnePair
      else                                        HandType.HighCard

    val kickers = ((1 until 5) flatMap groups.get).flatten.reverse
    require(hand.size == 5 && kickers.size == 5)
    (handType, if (isWheel) (kickers takeRight 4) :+ kickers.head else kickers)
  }
}

case class Counter() {
  val count = TrieMap[HandType.Value, Int]() withDefaultValue 0
  var total = 0
  def report(key: HandType.Value) { count(key) += 1; total += 1 }
}

class Analysis {
  var (expectedWin, expectedLoss) = (0d, 0d)
  val (wins, ties, losses) = (Counter(), Counter(), Counter())
  def total = wins.total + ties.total + losses.total

  def reportWin(handType: HandType.Value) { expectedWin += 1; wins report handType }
  def reportTie(handType: HandType.Value, split: Int) { expectedWin += 1d/split; ties report handType}
  def reportLoss(handType: HandType.Value) { expectedLoss += 1; losses report handType }

  def expectedReturn(canWin: Double, canLoss: Double) = (canWin*expectedWin - canLoss*expectedLoss)/total

  override def toString = {
    def percent(n: Int) = f"${100d * n /total}%5.2f%"
    def bucketDisplay(c: Counter) = (c.count map {e => f"${e._1}%15s: ${percent(e._2)}%s"}) mkString "\n"
    def totalDisplay(name: String, c: Counter) = s" $name: ${percent(c.total)}\n${bucketDisplay(c)}"
    s"${totalDisplay("Wins", wins)}\n${totalDisplay("Ties", ties)}\n${totalDisplay("Loss", losses)}"
  }
}

object Analyzer {
  import scala.math.Ordering.Implicits._

  implicit val rankOrdering = Ordering by {hand: Hand => (hand.handType, hand.sorted)}

  private val bestCache = TrieMap[Set[Card], Hand]()
  private def generateAll(hand: Set[Card]) = for (c1 <- hand; c2 <- hand; if c1 != c2) yield new Hand(hand - (c1, c2))
  def selectBest(hand: Set[Card]) = bestCache getOrElseUpdate (hand, generateAll(hand).max)

  def evaluate(myHand: Set[Card], board: Set[Card], otherPlayers: Int, simulations: Int = 10000) = {
    val analysis = new Analysis()
    for(simulation <- 1 to simulations) {
      val deck = new Deck()
      deck remove (myHand ++ board)
      val community = mutable.Set() ++ board
      val players = (1 to otherPlayers) map {i => Set(deck.deal, deck.deal)}
      while (community.size < 5) community += deck.deal

      val myBest = selectBest (myHand ++ community)
      val otherBests = players map {p => selectBest (p ++ community)}
      val matchUps = otherBests groupBy {p => rankOrdering.compare(myBest, p) signum}

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
