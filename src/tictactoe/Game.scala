package tictactoe

import fj.F
import fj.P
import fj.P1
import scala.annotation.tailrec
import Player._

class Game(p1: Strategy, p2: Strategy) {
  //val player2 = RandomMoves
  //val player1 = FirstAvailableMove
  //val player2 = FirstAvailableMove

  private def nextMove(b: Board) = {
    b.whoseTurn match {
      case Player1 => p1.nextPosition(b)
      case Player2 => p2.nextPosition(b)
    }
  }
  private def occupied(b: Board, p: Position): P1[Board.FinishedBoard] = {
    val lb: Board.FinishedBoard = null
    P.p(lb)
  }
  private val keepPlay: F[Board,Board.FinishedBoard] = new F[Board,Board.FinishedBoard]() {
    def f(b: Board): Board.FinishedBoard = {
      error("reached unreachable")
      val lb: Board.FinishedBoard = null
      lb
    }
  }
  private val gOver: F[Board.FinishedBoard,Board.FinishedBoard] = new F[Board.FinishedBoard,Board.FinishedBoard]() {
    def f(b: Board.FinishedBoard) = b
  }
  @tailrec
  private def playOut(b: Board, p: Position, mr: MoveResult): Board.FinishedBoard = {
    if (mr.keepPlaying.isNone) {
      mr.fold(occupied(b, p), keepPlay, gOver)
    } else {
      val nb = mr.keepPlaying.some
      //printBoard(nb)
      val np = nextMove(nb)
      val nr = nb.moveTo(np)
      playOut(nb, np, nr)
    }
  }
  def play: Board.FinishedBoard = {
    val fb = p1.firstMove
    val pos = nextMove(fb)
    val mr = fb.moveTo(pos)
    playOut(fb, pos, mr)
  }
}
