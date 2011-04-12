package tictactoe

import fj.F
import fj.P

object JohnDoe extends Strategy {

  type Weight = (Int,Position)

  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(Position.C)
  }
  private def neutral(p: Position): F[Board,Weight] = {
    new F[Board,Weight]() {
      def f(b: Board) = (0,p)
    }
  }
  private def gameOver(p: Position): F[Board.FinishedBoard,Weight] = {
    new F[Board.FinishedBoard,Weight]() {
      def f(b: Board.FinishedBoard) = (1,p)
    }
  }

  private def weighPosition(b: Board, p: Position): Weight = {
    b.moveTo(p).fold(P.p((Int.MinValue,p)),neutral(p),gameOver(p))
  }
  def nextPosition(b: Board): Position = {
    if (b.playerAt(Position.C).isNone) {
      return Position.C
    }
    val weights = Position.values.map {weighPosition(b, _)
    }.sortWith {_._1 < _._1}.reverse
    return weights(0)._2
  }
}
