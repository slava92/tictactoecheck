package ttt

import tictactoe.BoardLike
import tictactoe.Game
import tictactoe.GameResult._
import tictactoe.RandomMoves

object PlayTest {

  def main(args: Array[String]): Unit = {
    //val game = new Game(RandomMoves, FirstAvailableMove) // p1: 494 wins, p2: 89 wins, draw: 417 times
    //val game = new Game(FirstAvailableMove, RandomMoves) // p1: 255 wins, p2: 181 wins, draw: 564 times
    val game = new Game(RandomMoves, RandomMoves) // p1: 371 wins, p2: 290 wins, draw: 339 times
    val r1 = (0 until 1000).map {_ => game.play}.foldLeft((0,0,0)) { (sts, gr) => sts match {
        case (p1,p2,dr) => gr.result match {
            case Player1Wins => (p1+1,p2,dr)
            case Player2Wins => (p1,p2+1,dr)
            case Draw        => (p1,p2,dr+1)
        }
      }
    }
    r1 match {
      case (p1,p2,dr) => printf("p1: %d wins, p2: %d wins, draw: %d times\n", p1, p2, dr)
    }
  }

  def printBoard(b: BoardLike) {
    print(b.toString(ttt.Main.simpleCharsF))
  }
}
