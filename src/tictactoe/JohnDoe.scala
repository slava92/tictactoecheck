package tictactoe

import fj.F
import fj.P

object JohnDoe extends Strategy {
  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(Position.C)
  }
  private def neutral(p: Position): F[Board,(Int,Position)] = {
    new F[Board,(Int,Position)]() {
      def f(b: Board) = (0,p)
    }
  }
  private def gameOver(p: Position): F[Board.FinishedBoard,(Int,Position)] = {
    new F[Board.FinishedBoard,(Int,Position)]() {
      def f(b: Board.FinishedBoard) = (1,p)
    }
  }

  def nextPosition(b: Board): Position = {
    if (b.playerAt(Position.C).isNone) {
      return Position.C
    }
    val weights = Position.values.map {
      p => b.moveTo(p).fold(P.p((Int.MinValue,p)),neutral(p),gameOver(p))
    }.sortWith { _._1 < _._1
    }.reverse
    return weights(0)._2
//    if (0 < weights(0)._1) {
//      return weights(0)._2
//    } else {
//      return RandomMoves.nextPosition(b)
//    }
  }
}
