import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global

import Card.Implicits._

object ConsoleApp extends App {

  while(true) {
    try {
      println("--------------------------------------------------------------------")
      print("Enter hand (e.g AH TS): ")
      val myHand: Set[Card] = Console.readLine
      println(s"You are holding: $myHand")
      require(myHand.size == 2, "You must have 2 unique cards in hand")
      print("Enter community: ")
      val board: Set[Card] = Console.readLine
      println(s"Community Cards: $board")
      require(Array(0,3,4,5) exists {board.size == _}, "Community must have 0 or 3 or 4 or 5 unique cards")
      require(board intersect myHand isEmpty, "You cannot hold community cards")
      print("Enter number of players (excluding you) who have not folded yet: ")
      val otherPlayers = Console.readInt
      require(otherPlayers > 0, "Atleast one other player needed")
      val evaluation = future { Analyzer.evaluate(myHand, board, otherPlayers) }
      print("Enter current pot size: $")
      val possibleWins = Console.readDouble
      print("Enter your bets so far: $")
      val possibleLoss = Console.readDouble
      evaluation onSuccess {
        case analysis: Analysis => {
          val expectedReturn = analysis expectedReturn (possibleWins, possibleLoss)
          val percentReturn = 100 * expectedReturn/possibleLoss
          println(f"$analysis\nExpected return: $$$expectedReturn%6.3f ($percentReturn%5.2f%)")
        }
      }
      Console readLine
    } catch {
      case e: Exception => Console.err.println(s"Invalid input: ${e.getMessage}. Try again")
    }
  }
}