package ttt

import tictactoe.Game
import tictactoe.GameResult._
import tictactoe.RandomMoves
import tictactoe.JohnDoe
import tictactoe.JaneDoe
import tictactoe.JoeMnemonic
import tictactoe.Strategy
import tictactoe.FixedPoint
import tictactoe.Board
import fj.F
import fj.P
import fj.P1

object PlayTest {

  def main(args: Array[String]): Unit = {
    //val s1 = JohnDoe
    val s1 = RandomMoves
    //val s2 = JaneDoe
    //val s2 = JohnDoe
    val s2 = JoeMnemonic
    //oneGame(s1,s2)
    tourney(s1, s2, 1000)
  }

  def oneGame(s1: Strategy, s2: Strategy) {
    val rslt = new Game(s1,s2).playIt
    printTrace(rslt)
  }
  def tourney(s1: Strategy, s2: Strategy, runs: Int) {
    val pits = (s1,s2) :: (s2,s1) :: Nil
    //val pits = (s1,s2) :: Nil
    val rss = pits.map { _ match {
        case (p1, p2) =>
          val game = new Game(p1, p2)
          val r = (0 until runs).map {_ => game.playIt}.foldLeft((0,0,0)) { (stats, gr) => stats match {
              case (p1,p2,dr) => gr.result match {
                  case Player1Wins => (p1+1,p2,  dr)
                  case Player2Wins => (p1,  p2+1,dr)
                  case Draw        => (p1,  p2,  dr+1)
                }
            }
          }
          r
      }
    }
    val rs = (rss(0)._1+rss(1)._2,rss(0)._2+rss(1)._1,rss(0)._3+rss(1)._3)
    printf("p1: %d wins, p2: %d wins, draw: %d times\n", rs._1, rs._2, rs._3)
  }

  val emptyList: P1[List[Board]] = P.p(Nil)
  val moveBack: F[Board,List[Board]] = FixedPoint.fjF { nb: Board =>
    nb :: (nb.takeBack.fold(emptyList,moveBack))
  }
  def printTrace(rslt: Board.FinishedBoard) {
    val b = rslt.takeBack
    val bs = b :: b.takeBack.fold(emptyList,moveBack)
    bs.reverse foreach { b => FixedPoint.printBoard(b); println }
    FixedPoint.printBoard(rslt)
    println(rslt.result.toString)
  }
}
