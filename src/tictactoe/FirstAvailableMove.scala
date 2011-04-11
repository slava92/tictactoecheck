package tictactoe

import scala.collection.mutable.{Set => MSet}

object FirstAvailableMove extends Strategy {
  def firstMove(): Board = {
    val firstPos = Position.values.apply(0)
    Board.EmptyBoard.empty.moveTo(firstPos)
  }
  def nextPosition(b: Board): Position = {
    val opsi = b.occupiedPositions.toCollection.iterator
    val ops: MSet[Position] = MSet.empty
    while (opsi.hasNext) {
      ops.add(opsi.next)
    }
    val aps: Set[Position] = Position.values.toSet
    val fps = (aps -- ops).toList
    fps.head
  }
}
