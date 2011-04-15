package ttt

import tictactoe.BoardLike
import tictactoe.Game
import tictactoe.GameResult._
import tictactoe.RandomMoves
//import tictactoe.JohnDoe
import tictactoe.JaneDoe
import tictactoe.Strategy

object PlayTest {

  def main(args: Array[String]): Unit = {
    //val s1 = JohnDoe
    val s1 = JaneDoe
    val s2 = RandomMoves
    //tourney(s1, s2, 1000)
    oneGame(s2,s1)
  }

  def oneGame(s1: Strategy, s2: Strategy) {
//    val result = new Game(s1,s2).playTrace { b =>
//      printBoard(b)
//      println
//    }
    val result = new Game(s1,s2).playIt
    printBoard(result)
    println(result.result.toString)
  }
  def tourney(s1: Strategy, s2: Strategy, runs: Int) {
    val pits = (s1,s2) :: (s2,s1) :: Nil
    val rss = pits.map { _ match {
        case (p1, p2) =>
          val game = new Game(p1, p2)
          val r = (0 until runs).map {_ => game.play}.foldLeft((0,0,0)) { (sts, gr) => sts match {
              case (p1,p2,dr) => gr.result match {
                  case Player1Wins => (p1+1,p2,  dr)
                  case Player2Wins => (p1,  p2+1,dr)
                  case Draw        => (p1,  p2,  dr+1)
                }
            }
          }
          r
      }
    } //.foldLeft((0,0,0)) { (z,c) => (z._1+c._1,z._2+c._2,z._3+c._3) }
    val rs = (rss(0)._1+rss(1)._2,rss(0)._2+rss(1)._1,rss(0)._3+rss(1)._3)
    printf("p1: %d wins, p2: %d wins, draw: %d times\n", rs._1, rs._2, rs._3)
  }
  def printBoard(b: BoardLike) {
    print(b.toString(ttt.Main.simpleCharsF))
  }
}
