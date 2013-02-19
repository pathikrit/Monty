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
  override def toString = cards mkString ","
}

class Hand(hand: Iterable[Card]) {
  val (handType, sorted) = {
    require(hand.size == 5)

    val (rankGroups, suitGroups) = (hand groupBy {_.rank}, hand groupBy {_.suit})
    val ranks = rankGroups.keySet

    def hasSameRanks(matches: Int = 2, required: Int = 1) = (rankGroups count {_._2.size == matches}) >= required

    val isFlush = suitGroups.size == 1
    val isWheel = "A2345" forall {r => ranks contains (Card.ranks indexOf r)}   // A,2,3,4,5 straight
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
  object Type extends Enumeration {
    val HighCard, OnePair, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush = Value
  }

  import scala.math.Ordering.Implicits._

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
  type HT = Hand.Type.Value
  def newTypeCounter = TrieMap[HT, Int]().withDefaultValue(0)

  def evaluate(myHand: Set[Card], board: Set[Card], otherPlayers: Int, simulations: Int = 1000) = {
    val stats = new Stats()
    (1 to simulations).par foreach (simulation => {
      val deck = new Deck()
      (myHand ++ board) foreach {card: Card => deck.cards dequeueFirst (_ == card)}
      val community = mutable.Set() ++ board
      val players = (1 to otherPlayers) map {i => Set(deck.deal, deck.deal)}
      while (community.size < 5) community += deck.deal

      val myBest = Hand.selectBest(myHand ++ community)
      val otherBests = players map {p => Hand.selectBest(p ++ community)}
      val matchUps = otherBests groupBy {p => Hand.ordering.compare(myBest, p).signum}

      myBest.handType match {
        case defeat if matchUps contains -1 => stats.logLoss(matchUps(-1).max.handType)
        case victory if !(matchUps contains 0) => stats.logWin(victory)
        case tied => stats.logTie(tied, matchUps(0).size)
      }
    })
    stats
  }
}

object Monty extends App {

  def fromCommaSeparated(s: String) = if (s.isEmpty) Array.empty[Card] else (s split "," map {c => Card from c.trim})
  def readCardsFromConsole = fromCommaSeparated(Console.readLine()).toSet

  while(true) {
    try {
      println("---------------------------------")
      print("Enter hand comma-separated (e.g AH, TS): ")
      val myHand = readCardsFromConsole
      println(s"You are holding: $myHand")
      require(myHand.size == 2, "You must have 2 unique cards in hand")
      print("Enter community cards (in similar comma separated format): ")
      val board = readCardsFromConsole
      println(s"Community Cards: $board")
      require(Array(0,3,4,5) exists {board.size == _}, "Community must have 0 or 3 or 4 or 5 unique cards")
      require(board intersect myHand isEmpty, "You cannot hold community cards")
      print("Enter number of players (excluding you) who have not folded yet: ")
      val otherPlayers = Console.readInt()
      require(otherPlayers > 0, "Atleast one other player needed")
      print(Stats.evaluate(myHand, board, otherPlayers))
    } catch {
      case e: Exception => Console.err.println(s"Invalid input: ${e.getMessage}. Try again")
    }
  }
}
