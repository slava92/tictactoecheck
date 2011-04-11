package tictactoe

import fj.F
import fj.P
import org.scalacheck._

object MoveResultCheck {
  def keepPl(r: Boolean): F[Board,Boolean] = {
    new F[Board,Boolean]() { def f(b: Board) = r }
  }
  def gameOv(r: Boolean): F[Board.FinishedBoard,Boolean] = {
    new F[Board.FinishedBoard,Boolean]() { def f(b: Board.FinishedBoard) = r }
  }
  val moveResults = Gen.choose(0,2).map {
    _ match {
      case 0 => (MoveResult.positionAlreadyOccupied,P.p(true), keepPl(false),gameOv(false))
      case 1 => (MoveResult.keepPlaying(null),      P.p(false),keepPl(true), gameOv(false))
      case 2 => (MoveResult.gameOver(null),         P.p(false),keepPl(false),gameOv(true))
    }
  }
  val propFold = Prop.forAll(moveResults) {
    _ match {
      case (mr,pao,kp,go) => mr.fold(pao,kp,go)
    }
  }

  def main(args: Array[String]): Unit = {
    PlayerCheck.checkProp(propFold)
  }

}
