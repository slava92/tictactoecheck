package tictactoe

import fj.F
import fj.P
import fj.P1
import fj.data.{Either => Er}
import Player._

class Game(p1: Strategy, p2: Strategy) {

  private def nextMove(b: Board) = {
    b.whoseTurn match {
      case Player1 => p1.nextPosition(b)
      case Player2 => p2.nextPosition(b)
    }
  }

  private def wrongMove(b: Board, p: Position): P1[Er[String,Board.FinishedBoard]] = {
    ///ttt.PlayTest.printBoard(b)
    P.p(Er.left("Player "++b.whoseTurn.toString++" attempted to move to occupied position "++p.toString))
  }

  private val gameOver = new F[Board.FinishedBoard,Er[String,Board.FinishedBoard]]() {
    def f(b: Board.FinishedBoard) = Er.right(b)
  }

  private val keepPlaying: F[Board,Er[String,Board.FinishedBoard]] = new F[Board,Er[String,Board.FinishedBoard]]() {
    def f(b: Board): Er[String,Board.FinishedBoard] = {
      val p = nextMove(b)
      b.moveTo(p).fold(wrongMove(b,p), keepPlaying, gameOver)
    }
  }

  def playIt: Board.FinishedBoard = {
    val er = keepPlaying.f(p1.firstMove)
    if (er.isLeft) {
      error(er.left.value)
    } else {
      er.right.value
    }
  }

  def makeTraceBoard(trace: BoardLike => Unit, moveOn: => F[Board,Er[String,Board.FinishedBoard]]): F[Board,Er[String,Board.FinishedBoard]] =
    new F[Board,Er[String,Board.FinishedBoard]]() {
      def f(b: Board): Er[String,Board.FinishedBoard] = {
        trace(b)
        val p = nextMove(b)
        b.moveTo(p).fold(wrongMove(b,p), moveOn, gameOver)
      }
    }

  def traceIt(trace: BoardLike => Unit): Board.FinishedBoard = {
    var tb: F[Board,Er[String,Board.FinishedBoard]] = null
    tb = makeTraceBoard(trace, tb)
    val er = tb.f(p1.firstMove)
    if (er.isLeft) {
      error(er.left.value)
    } else {
      er.right.value
    }
  }
}
