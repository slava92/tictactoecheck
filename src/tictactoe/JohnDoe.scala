package tictactoe

import fj.F
import fj.P

object JohnDoe extends Strategy {

  type Weight = (Int,Position)
  val BigBang = 100

  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(Position.C)
  }
  private def neutral(p: Position, s: Int): F[Board,Weight] = {
    new F[Board,Weight]() {
      def f(b: Board) = {
        val weights = Position.values.map {weighPosition(b, _, (-s))
        }.sortWith {_._1 < _._1}.reverse
        //(0,p)
        weights(0)
      }
    }
  }
  private def gameOver(p: Position, s: Int): F[Board.FinishedBoard,Weight] = {
    new F[Board.FinishedBoard,Weight]() {
      def f(b: Board.FinishedBoard) = ((BigBang-b.nmoves())*s,p)
    }
  }

  private def weighPosition(b: Board, p: Position, s: Int): Weight = {
    b.moveTo(p).fold(P.p((Int.MinValue,p)),neutral(p,s),gameOver(p,s))
  }
  def nextPosition(b: Board): Position = {
    if (b.playerAt(Position.C).isNone) {
      return Position.C
    }
    val weights = Position.values.map {weighPosition(b, _, 1)
    }.sortWith {_._1 < _._1}.reverse
//    weights.foreach { wp =>
//      printf("%2s %s\n", wp._2.toString, wp._1.toString)
//    }
    weights(0)._2
  }
}
