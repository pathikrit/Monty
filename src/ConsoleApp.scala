object ConsoleApp extends App {

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