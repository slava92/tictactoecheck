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
  val hm0: HashMap[String,Position] = HashMap.empty

  def endGame(p: Position): F[Board.FinishedBoard,(HashMap[String,Position], Int, Position)] = fjF { eb =>
    val hmf = hm0 + (board2key(eb) -> p)
    if (eb.result.isDraw) (hmf,0,p) else (hmf,BigBang - eb.nmoves,p)
  }
  def newPos: F[Board,(HashMap[String,Position], Int, Position)] = fjF {b =>
    val nextLevel = freeSpots(b).map { p =>
      b.moveTo(p).fold(
        P.p((hm0,Int.MinValue,p)),
        newPos,
        endGame(p)
      )
    }.sortWith {_._2 < _._2}.reverse
    val recommend = nextLevel(0)._3
    val hmn: HashMap[String,Position] = nextLevel.foldLeft(hm0) { (hm, tpl) =>
      hm ++ tpl._1
    } + (board2key(b) -> recommend)
    (hmn, nextLevel(0)._2, recommend)
  }

  def collectMoves = {
    val bs: Array[(HashMap[String,Position], Int, Position)] = Position.values.take(1).map { p =>
      printf("Analyzing %c (%s)\n", p.toChar, board2key(Board.EmptyBoard.empty.moveTo(p)))
      val np = newPos.f(Board.EmptyBoard.empty.moveTo(p))
      (np._1, np._2, p)
    }.sortWith {_._2 < _._2}.reverse
    val recommend = bs(0)._3
    val hmn: HashMap[String,Position] = bs.foldLeft(hm0) { (hm, tpl) =>
      hm ++ tpl._1
    } + ("<0>" -> recommend)
    (hmn, bs(0)._2, recommend)
  }

  def board2key(b: BoardLike): String = {
    val bs = b.occupiedPositions.foldLeft(
      new F2[String,Position,String] { def f(z: String, p0: Position) = {
          z + p0.toChar + b.playerAt(p0).some().toSymbol
        }
      },
      "")
    bs
  }

  def main(args: Array[String]): Unit = {
    collectMoves match {
      case (hm, wt, p) =>
	printf("First move is %c (%d)\n", p.toChar, wt)
	print(hm.mkString("\n"))
    }
  }
}
