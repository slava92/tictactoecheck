package tictactoe

import scala.util.Random
import scala.collection.mutable.{Set => MSet}

object RandomMoves extends Strategy {
  def firstMove(): Board = {
    val firstPos = Position.fromInt(Random.nextInt(Position.values.size)+1).some
    Board.EmptyBoard.empty.moveTo(firstPos)
  }
  def nextPosition(b: Board): Position = {
    val opsi = b.occupiedPositions.toCollection.iterator
    val ops: MSet[Position] = MSet.empty
    while (opsi.hasNext) {
      ops.add(opsi.next)
    }
    val aps: Set[Position] = Position.values.toSet
    val fps: Array[Position] = (aps -- ops).toArray
    fps(Random.nextInt(fps.size))
  }
}
