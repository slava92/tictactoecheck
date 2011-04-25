package tictactoe

import fj.F
import fj.F2
import fj.P
import fj.data.{TreeMap => TM}
import fj.Ord
import java.lang.{Integer => JI}
import scala.collection.immutable.HashMap
import scala.util.Random

import Game.freeSpots
import FixedPoint.fjF
import Position._
import Player._

object JoeMnemonic extends Strategy {
  val rnd = new Random(System.currentTimeMillis)
  def firstMove(): Board = {
    val moves = game("00")
    val mi = rnd.nextInt(moves.size)
    Board.EmptyBoard.empty.moveTo(moves.toList(mi))
  }
  def nextPosition(b: Board): Position = {
    val moves = game(board2key(b))
    val mi = rnd.nextInt(moves.size)
    moves.toList(mi)
  }

  type GameTree = HashMap[String,Set[Position]]
  type Weight = Int
  val BigBang = 100
  val hm0: GameTree = HashMap.empty

  def endGame(p: Position): F[Board.FinishedBoard,(GameTree, Weight, Position)] = fjF { eb =>
    val hmf = hm0 + (board2key(eb) -> Set(p))
    if (eb.result.isDraw) (hmf,0,p) else (hmf,BigBang - eb.nmoves,p)
  }
  def newPos: F[Board,(GameTree, Weight, Position)] = fjF {b =>
    val nextMoves = freeSpots(b).map { p =>
      b.moveTo(p).fold(
        P.p((hm0,Int.MinValue,p)),
        newPos.andThen(fjF {tp=>(tp._1,0-tp._2,p)}),
        endGame(p)
      )
    }.sortWith {_._2 < _._2}.reverse
    val r0 = nextMoves.span(_._2 == nextMoves(0)._2)
    val recommend = r0._1.toSet
    val hmn: GameTree = nextMoves.foldLeft(hm0+(board2key(b)->recommend.map {_._3})) { (hm, tpl) =>
      joinMaps(hm, tpl._1)
    }
    (hmn, nextMoves(0)._2, recommend.head._3)
  }
  def collectMoves = {
    val bs: Array[(GameTree, Weight, Position)] = Position.values.take(100).map { p =>
      //val p = Position.SW
      val firstBoard = Board.EmptyBoard.empty.moveTo(p)
      printf("Analyzing %c (%s)\n", p.toChar, board2key(firstBoard))
      newPos.andThen(fjF {tp=>(tp._1,tp._2,p)}).f(firstBoard)
    }.sortWith {_._2 < _._2}.reverse
    val r0 = bs.span(_._2 == bs(0)._2)
    val recommend = r0._1.toSet
    val hmn: GameTree = bs.foldLeft(hm0+("00"->recommend.map {_._3})) { (hm, tpl) =>
      joinMaps(hm, tpl._1)
    }
    (hmn, bs(0)._2, recommend.map {_._3})
  }
  def joinMaps(m1: GameTree, m2: GameTree): GameTree = {
    m2.foldLeft(m1) { (m, kv) =>
      updateMap(m, kv._1, kv._2)
    }
  }
  def updateMap(m: GameTree, k: String, v:Set[Position]): GameTree = {
    m + (k -> (v ++ m.getOrElse(k, Set.empty)))
  }
  def board2key(b: BoardLike): String = {
    b.occupiedPositions.foldLeft(
      new F2[String,Position,String] { def f(z: String, p0: Position) = {
          p0.toChar +: b.playerAt(p0).some().toSymbol +: z
        }
      }
      , "")
  }

  val game: GameTree = {
    val gameIn = JoeMnemonic.getClass().getResourceAsStream("JoeMnemonic.game")
    scala.io.Source.fromInputStream(gameIn).getLines.foldLeft(hm0) { (b, a) =>
      val ts = a.split(' ')
      if (2 == ts.size) {
	b + (ts(0) -> ts(1).split(',').map(Position.valueOf(_)).toSet)
      } else {
	printf("Malformed game line: '%s'\n", a)
	b
      }
    }
  }

  def main(args: Array[String]): Unit = {
    if (true) {
      println(game.size)
    }
    if (false) {
      val m1 = updateMap(hm0, "0", (Set(1,2).map {Position.fromInt(_).some}))
      val m2 = updateMap(hm0, "0", (Set(3,4).map {Position.fromInt(_).some}))
      println(joinMaps(m2,m1).mkString("\n"))
      val m3 = updateMap(m1, "0", (Set(5,6).map {Position.fromInt(_).some}))
      println(m3.mkString("\n"))
    }
    if (false) {
// X O X
// _ O _
// O _ X
      val pl: List[(Position,Player)] = (NW,Player1)::(N,Player2)::(NE,Player1)::(C,Player2)::(SW,Player2)::(SE,Player1)::Nil
      val nextTurn = Player1
      //val pl: List[(Position,Player)] = (SW,Player1)::Nil
      //val nextTurn = Player2

      val tme: TM[JI,Player] = TM.empty(Ord.intOrd)
      val tms = pl.foldLeft(tme) { (tm,p) => tm.set(p._1.toInt,p._2) }
      val pb: fj.data.Option[Board] = fj.data.Option.none()
      val tb = new Board(nextTurn, tms, 6, pb)
      FixedPoint.printBoard(tb)
      val r: (GameTree, Weight, Position) = newPos.f(tb)
      r match {
	case (hm, wt, p) =>
	  printf("First move is %c (%d)\n", p.toChar, wt)
	  hm.foreach { _ match {
	    case (k,v) => printf("%s %s\n", k, v.mkString(","))
	  }
	}
      }
    }

    if (true) {
      collectMoves match {
	case (hm, wt, ps) =>
	  printf("First move is %c (%d)\n", ps.head.toChar, wt)
	  //print(hm.mkString("\n"))
	  hm.foreach { _ match {
	    case (k,v) => printf("%s %s\n", k, v.mkString(","))
	  }
	}
      }
    }
  }
}
