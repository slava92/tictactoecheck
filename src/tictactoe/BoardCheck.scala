package tictactoe

import Position._
import Player._
import fj.F
import fj.Ord
import fj.P
import fj.data.{TreeMap => TM}
import java.lang.{Integer => JI}
import org.scalacheck.Prop._
import org.scalacheck._

object BoardCheck {
  val rs = RandomMoves;
  val propFM = Prop.forAll { (n: Int) =>
    val fm = rs.firstMove //firstMove()
    (fm.whoseTurn == Player.Player2)   :| "next is Player2" &&
    (fm.occupiedPositions.length == 1) :| "one player" &&
    (fm.isEmpty == false)              :| "not empty" &&
    (fm.playerAt(fm.occupiedPositions.head).some == Player.Player1) :| "player 1"
  }
  val secondMove: (Board) => MoveResult = (b: Board) => {
    b.moveTo(rs.nextPosition(b))
  }
  def secondBoard(): F[Board,Boolean] = {
    new F[Board,Boolean]() {
      def f(b: Board) = {
        b.occupiedPositions.length == 2 &&
        b.occupiedPositions.head != b.occupiedPositions.last &&
        b.whoseTurn == Player.Player1
      }
    }
  }
  val propSM = Prop.forAll { (n: Int) =>
    val sm = secondMove(rs.firstMove()) //firstMove())
    sm.fold(P.p(false), secondBoard(),MoveResultCheck.gameOv(false))
  }
  def main(args: Array[String]): Unit = {
    PlayerCheck.checkProp(propFM)
    PlayerCheck.checkProp(propSM)
    tryMove
  }
  def tryMove {
//Player2 attempted to move to NE
//_ O _
//X X O
//X O X
    //val pl: List[(Position,Player)] = (N,Player2) :: (W,Player1) :: (C,Player1) :: (E,Player2) :: (SW,Player1) :: (S,Player2) :: (SE,Player1) :: Nil
//Player2 attempted to move to W
//X _ _
//_ _ _
//O X _
    //val pl: List[(Position,Player)] = (NW,Player1) :: (SW,Player2) :: (S,Player1) :: Nil
//Player2 attempted to move to N
//O _ O
//X _ X
//O X X
    val pl: List[(Position,Player)] = (NW,Player2) :: (NE,Player2) :: (W,Player1) :: (E,Player1) :: (SW,Player2) :: (S,Player1) :: (SE,Player1) :: Nil
    val tme: TM[JI,Player] = TM.empty(Ord.intOrd)
    val tms = pl.foldLeft(tme) { (tm,p) => tm.set(p._1.toInt,p._2) }
    val tb = new Board(Player.Player2, tms, 1)
    printBoard(tb)
    val mr = tb.moveTo(N)
    val r = mr.fold(P.p(false), MoveResultCheck.keepPl(false), gameOv(true))
    println("result of move to N = "++r.toString)
  }
  def gameOv(r: Boolean): F[Board.FinishedBoard,Boolean] = {
    new F[Board.FinishedBoard,Boolean]() { def f(b: Board.FinishedBoard) = {
        printBoard(b); println("game result = "++b.result.toString)
        r
      }
    }
  }

  def printBoard(b: BoardLike) {
    print(b.toString(ttt.Main.simpleCharsF))
    println("Next move: "++b.whoseTurn.toString)
  }
}
