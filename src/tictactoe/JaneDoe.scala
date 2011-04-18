package tictactoe

import fj.F
import fj.Ord
import fj.P
import Board._
import Position._
import Player._
import Game.freeSpots
import FixedPoint.fjF
import fj.data.{TreeMap => TM}
import java.lang.{Integer => JI}

object JaneDoe extends Strategy {

  type Weight = (Int,Position)
  val BigBang = 10
  val corners = NW :: NE :: SW :: SE :: Nil

  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(C)
  }
  def nextPosition(b: Board): Position = {
    if (b.playerAt(C).isNone) {
      C
    } else {
      val weights = freeSpots(b).map {weighPosition(b, _)}.sortWith {
        _._1 < _._1
      }.reverse
      // position with highest weight
      //weights.map { bp => printf("NP: %s=%d\n", bp._2.toString, bp._1)}
      weights(0)._2
    }
  }

  private def weighPosition(b: Board, p: Position): Weight = {
    b.moveTo(p).fold(
      P.p((Int.MinValue,p)),
      weighNeutralMove(p),
      fjF {b => if (b.result.isDraw) (0,p) else (BigBang - b.nmoves,p)}
    )
  }
  private def weighNeutralMove(p: Position): F[Board,Weight] = fjF { b =>
    val isWinner: Boolean = freeSpots(b).map { p2 =>
      b.moveTo(p2).fold(P.p(false), fjF(_=>false), fjF(_.result.isWin))
    }.contains(true)
    if (isWinner) {
      (b.nmoves-BigBang,p)
    } else if (corners.contains(p)) {
      (2,p) // corners are perferable over sides
    } else {
      (1,p)
    }
  }

  def main(args: Array[String]): Unit = {
// _ _ _
// O X _
// X _ _
    //val pl: List[(Position,Player)] = (W,Player2) :: (C,Player1) :: (SW,Player1) :: Nil
    //val pl: List[(Position,Player)] = (C,Player1) :: Nil
// X _ X
// _ O _
// O _ X
    //val pl: List[(Position,Player)] = (NW,Player1)::(NE,Player1)::(C,Player2)::(SW,Player2)::(SW,Player1)::Nil
    val pl: List[(Position,Player)] = (NW,Player1) :: (C,Player2) :: (SE,Player1) :: Nil
    val tme: TM[JI,Player] = TM.empty(Ord.intOrd)
    val tms = pl.foldLeft(tme) { (tm,p) => tm.set(p._1.toInt,p._2) }
    val tb = new Board(Player.Player2, tms, 5)
    //ttt.PlayTest.printBoard(tb)
    printf("next move for %s: %s\n", tb.whoseTurn.toString, JaneDoe.nextPosition(tb).toString)
  }
}
