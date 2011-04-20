package tictactoe

import fj.F
import fj.F2
import fj.P
import fj.data.{TreeMap => TM}
import fj.Ord
import java.lang.{Integer => JI}

import scala.collection.immutable.HashMap
import Game.freeSpots
import FixedPoint.fjF
import Position._
import Player._

object JoeMnemonic extends Strategy {
  def firstMove(): Board = {
    Board.EmptyBoard.empty.moveTo(NW)
  }
  def nextPosition(b: Board): Position = {
    game(board2key(b))
  }

  type Weight = (Int,Position)
  val BigBang = 100
  val hm0: HashMap[String,Position] = HashMap.empty

  def endGame(p: Position): F[Board.FinishedBoard,(HashMap[String,Position], Int, Position)] = fjF { eb =>
    val hmf = hm0 + (board2key(eb) -> p)
    if (eb.result.isDraw) (hmf,0,p) else (hmf,BigBang - eb.nmoves,p)
  }
  def keepPlay: F[Board,(HashMap[String,Position], Int, Position)] = fjF {b =>
    newPos.f(b) match {
      case (hm, wt, p) => (hm, 0-wt, p)
    }
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
    val bs: Array[(HashMap[String,Position], Int, Position)] = Position.values.take(100).map { p =>
      //val p = Position.NW
      printf("Analyzing %c (%s)\n", p.toChar, board2key(Board.EmptyBoard.empty.moveTo(p)))
      val np = newPos.f(Board.EmptyBoard.empty.moveTo(p))
      (np._1, np._2, p)
    }.sortWith {_._2 < _._2}.reverse
    val recommend = bs(0)._3
    val hmn: HashMap[String,Position] = bs.foldLeft(hm0) { (hm, tpl) =>
      hm ++ tpl._1
    } + ("00" -> recommend)
    (hmn, bs(0)._2, recommend)
  }

  def board2key(b: BoardLike): String = {
    val bs = b.occupiedPositions.foldLeft(
      new F2[String,Position,String] { def f(z: String, p0: Position) = {
          p0.toChar +: b.playerAt(p0).some().toSymbol +: z
        }
      },
      "")
    bs
  }

  val game = {
    val gameIn = JoeMnemonic.getClass().getResourceAsStream("JoeMnemonic.game")
    val game = scala.io.Source.fromInputStream(gameIn).getLines.foldLeft(hm0) { (b, a) =>
      val ts = a.split(' ')
      if (3 == ts.size) {
	b + (ts(0) -> Position.valueOf(ts(2)))
      } else {
	printf("Malformed game line: '%s'\n", a)
	b
      }
    }
    game
  }

  def main(args: Array[String]): Unit = {
    if (false) {
      //print(game.mkString("\n"))
      print(game.size)
    }
    if (false) {
// X O X
// _ O _
// O _ X
      val pl: List[(Position,Player)] = (NW,Player1)::(N,Player2)::(NE,Player1)::(C,Player2)::(SW,Player2)::(SE,Player1)::Nil
      val nextTurn = Player1
// X _ _ 
// _ X O 
// _ _ _ 
      //val pl: List[(Position,Player)] = (NW,Player1)::(C,Player1)::(E,Player2)::Nil
      //val nextTurn = Player2
      //val pl: List[(Position,Player)] = (NW,Player1)::Nil
      //val nextTurn = Player2

      val tme: TM[JI,Player] = TM.empty(Ord.intOrd)
      val tms = pl.foldLeft(tme) { (tm,p) => tm.set(p._1.toInt,p._2) }
      val tb = new Board(nextTurn, tms, 6)
      FixedPoint.printBoard(tb)
      val r: (HashMap[String,Position], Int, Position) = newPos.f(tb)
      r match {
	case (hm, wt, p) =>
	  printf("First move is %c (%d)\n", p.toChar, wt)
	print(hm.mkString("\n"))
      }
    }

    if (true) {
      collectMoves match {
	case (hm, wt, p) =>
	  printf("First move is %c (%d)\n", p.toChar, wt)
	print(hm.mkString("\n"))
      }
    }
  }
}
