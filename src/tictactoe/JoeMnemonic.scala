package tictactoe

import fj.F
import fj.F2
import fj.P
import scala.collection.immutable.HashMap
import Game.freeSpots
import FixedPoint.fjF

object JoeMnemonic {
  type Weight = (Int,Position)
  val BigBang = 100

  def collectMoves = {
    val eh: HashMap[Position,Board] = HashMap.empty
    val cm = Position.values.foldLeft(eh) { (z,p) =>
      z + ((p,Board.EmptyBoard.empty.moveTo(p)))
    }
    Position.values.foreach { p1 =>
      val fb = Board.EmptyBoard.empty.moveTo(p1)
      freeSpots(fb).foreach { p2 =>
        val w = weighPosition(fb, p2)
        printf("%s -> %s -> %s\n", board2key(fb), p2.toString, w)
      }
    }
    cm
  }

  private def weighPosition(b: Board, p: Position): Weight = {
    val isWinner: Boolean = b.moveTo(p).fold(P.p(false), fjF(_=>false), fjF(_.result.isWin))
    if (isWinner) {
      return (BigBang - b.nmoves,p)
    }
    val wp = b.moveTo(p).fold(
      P.p((Int.MinValue,p)),
      weighNeutralMove(p),
      fjF {b => if (b.result.isDraw) (0,p) else (BigBang - b.nmoves,p)}
    )
    wp
  }

   private def weighNeutralMove(p: Position): F[Board,Weight] = fjF { b =>
    val weights = freeSpots(b).map {n => weighPosition(b, n)
    }.sortWith {_._1 < _._1}.reverse
    (0-weights(0)._1,p)
  }

 def board2key(b: Board): String = {
    val bs = b.occupiedPositions.foldLeft(
      new F2[String,Position,String] { def f(z: String, p0: Position) = {
          z + p0.toChar
        }
      },
      "")
    bs
  }

  def main(args: Array[String]): Unit = {
    collectMoves.foreach { _ match {
        case (p, b) =>
          printf("%s moved to %s: %s\n", b.whoseNotTurn.toString, p.toString, board2key(b))
      }
    }
  }
}