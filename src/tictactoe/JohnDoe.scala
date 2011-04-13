package tictactoe

import fj.F
import fj.Ord
import fj.P
import Board._
import Position._
import Player._
import fj.data.{TreeMap => TM}
import java.lang.{Integer => JI}

object JohnDoe extends Strategy {

  type Weight = (Int,Position)
  val BigBang = 100

  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(Position.C)
  }
  private def neutral(p: Position, s: Int): F[Board,Weight] = {
    new F[Board,Weight]() {
      def f(b: Board): Weight = {
        val weights = Position.values.filter {b.playerAt(_).isNone
        }.map {n => weighPosition(b, n, (-s))
        }.sortWith {_._1 < _._1}.reverse
        (0-weights(0)._1,p)
      }
    }
  }
  private def gameOver(p: Position, s: Int): F[Board.FinishedBoard,Weight] = {
    new F[Board.FinishedBoard,Weight]() {
      def f(b: Board.FinishedBoard) = {
        if (b.result.isDraw)
          (0,p)
        else {
          error("Unreachable state has been reached")
//          if (-1 < b.nmoves) {
//            println("XXXXXXXXX")
//            ttt.PlayTest.printBoard(b)
//            println("XXXXXXXXX")
//          }
          ((BigBang-b.nmoves())*s,p)
        }
      }
    }
  }

  def keepPl[T](r: T): F[Board,T] = { new F[Board,T]() { def f(b: Board) = r } }
  def gameOv[T](r: FinishedBoard => T): F[FinishedBoard,T] =
  { new F[Board.FinishedBoard,T]() { def f(b: Board.FinishedBoard) = r(b) } }
  private def weighPosition(b: Board, p: Position, s: Int): Weight = {
    val isWinner: Boolean = b.moveTo(p).fold(P.p(false), keepPl(false), gameOv(_.result.isWin))
    if (isWinner) {
      return (BigBang - b.nmoves,p)
    }
    val wp = b.moveTo(p).fold(P.p((Int.MinValue,p)),neutral(p,-1),gameOver(p,1))
//    if (b.nmoves == 0) {
//      println("=========")
//      ttt.PlayTest.printBoard(b)
//      printf("%2s %s\n", wp._2.toString, wp._1.toString)
//      println("=========")
//    }
    wp
  }
  def nextPosition(b: Board): Position = {
    if (b.playerAt(Position.C).isNone) {
      return Position.C
    }
    val weights = Position.values.filter {b.playerAt(_).isNone
    }.map {p => weighPosition(b, p, 1)
    }.sortWith {_._1 < _._1}.reverse
    weights.foreach { wp =>
//      println("_________")
//      ttt.PlayTest.printBoard(b)
//      printf("%2s %s\n", wp._2.toString, wp._1.toString)
//      println("_________")
    }
//    printf("%2s %s\n", weights(0)._2.toString, weights(0)._1.toString)
    weights(0)._2
  }

  def main(args: Array[String]): Unit = {
// _ _ _
// O X _
// X _ _
    val pl: List[(Position,Player)] = (W,Player2) :: (C,Player1) :: (SW,Player1) :: Nil
    val tme: TM[JI,Player] = TM.empty(Ord.intOrd)
    val tms = pl.foldLeft(tme) { (tm,p) => tm.set(p._1.toInt,p._2) }
    val tb = new Board(Player.Player2, tms, 3)
    ttt.PlayTest.printBoard(tb)
    printf("next move for %s: %s\n", tb.whoseTurn.toString, JohnDoe.nextPosition(tb).toString)
  }
}
