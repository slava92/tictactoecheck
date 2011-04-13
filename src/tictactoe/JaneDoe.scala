package tictactoe

import fj.F
import fj.Ord
import fj.P
import Board._
import Position._
import Player._
import fj.data.{TreeMap => TM}
import java.lang.{Integer => JI}

object JaneDoe extends Strategy {

  type Weight = (Int,Position)
  val BigBang = 10
  val corners = NW :: NE :: SW :: SE :: Nil

  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(Position.C)
  }
  private def neutral(p: Position): F[Board,Weight] = {
    new F[Board,Weight]() {
      def f(b: Board): Weight = {
        val freeSpots = Position.values.filter {b.playerAt(_).isNone}
        val isWinner: Boolean = freeSpots.map { p2 =>
          b.moveTo(p2).fold(P.p(false), keepPl(false), gameOv(_.result.isWin))
        }.contains(true)
        if (isWinner) {
          (b.nmoves-BigBang,p)
        } else {
          if (corners.contains(p)) {
            (2,p) // corners are perferable over sides
          } else {
            (1,p)
          }
        }
      }
    }
  }
  private def gameOver(p: Position): F[Board.FinishedBoard,Weight] = {
    new F[Board.FinishedBoard,Weight]() {
      def f(b: Board.FinishedBoard) = {
        if (b.result.isDraw) {
          (0,p)
        } else {
          error("reached unreachable place")
          //(BigBang-b.nmoves(),p)
        }
      }
    }
  }

  def keepPl[T](r: T): F[Board,T] = { new F[Board,T]() { def f(b: Board) = r } }
  def gameOv[T](r: FinishedBoard => T): F[FinishedBoard,T] =
  { new F[Board.FinishedBoard,T]() { def f(b: Board.FinishedBoard) = r(b) } }
  private def weighPosition(b: Board, p: Position): Weight = {
    val isWinner: Boolean = b.moveTo(p).fold(P.p(false), keepPl(false), gameOv(_.result.isWin))
    if (isWinner) {
      (BigBang - b.nmoves,p)
    } else {
      b.moveTo(p).fold(P.p((Int.MinValue,p)),neutral(p),gameOver(p))
    }
  }
  def nextPosition(b: Board): Position = {
    if (b.playerAt(Position.C).isNone) {
      return Position.C
    }
    val freeSpots = Position.values.filter {b.playerAt(_).isNone}
    val weights = freeSpots.map {p => weighPosition(b, p)
    }.sortWith {_._1 < _._1}.reverse
//    weights.foreach { wp =>
//      printf(" > %s %s\n", wp._2.toString, wp._1.toString)
//    }
    // position with highest weight
    weights(0)._2
  }

  def main(args: Array[String]): Unit = {
// _ _ _
// O X _
// X _ _
    //val pl: List[(Position,Player)] = (W,Player2) :: (C,Player1) :: (SW,Player1) :: Nil
    val pl: List[(Position,Player)] = (C,Player1) :: Nil
    val tme: TM[JI,Player] = TM.empty(Ord.intOrd)
    val tms = pl.foldLeft(tme) { (tm,p) => tm.set(p._1.toInt,p._2) }
    val tb = new Board(Player.Player2, tms, 3)
    ttt.PlayTest.printBoard(tb)
    printf("next move for %s: %s\n", tb.whoseTurn.toString, JaneDoe.nextPosition(tb).toString)
  }
}
