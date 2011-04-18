package tictactoe

import fj.F
import fj.P
import fj.P1
import fj.data.{Either => Er}
import Board.FinishedBoard
import Player._
import FixedPoint.fjF

class Game(p1: Strategy, p2: Strategy) {

  def playIt: FinishedBoard = {
    val er = keepPlaying.f(p1.firstMove)
    if (er.isLeft) {
      error(er.left.value)
    } else {
      er.right.value
    }
  }

  def traceIt(trace: BoardLike => Unit): FinishedBoard = {
    val er = keepTracing(trace).f(p1.firstMove)
    if (er.isLeft) {
      error(er.left.value)
    } else {
      er.right.value
    }
  }

  private type EndResult = Er[String,FinishedBoard]

  private val keepPlaying: F[Board,EndResult] = new F[Board,EndResult]() {
    def f(b: Board): EndResult = {
      val p = nextMove(b)
      b.moveTo(p).fold(wrongMove(b,p), keepPlaying, gameOver)
    }
  }
  private val keepTracing: (BoardLike => Unit)=>F[Board,EndResult] = { trace =>
    new F[Board,EndResult]() {
      def f(b: Board): EndResult = {
        trace(b)
        val p = nextMove(b)
        b.moveTo(p).fold(wrongMove(b,p), keepTracing(trace), gameOver)
      }
    }
  }

  private def nextMove(b: Board) = {
    b.whoseTurn match {
      case Player1 => p1.nextPosition(b)
      case Player2 => p2.nextPosition(b)
    }
  }

  private def wrongMove(b: Board, p: Position): P1[EndResult] = {
    P.p(Er.left("Player "++b.whoseTurn.toString++" attempted to move to occupied position "++p.toString))
  }
  private val gameOver: F[FinishedBoard,EndResult] = fjF {Er.right(_)}
}
