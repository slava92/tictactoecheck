package tictactoe

trait Strategy {
  def firstMove(): Board
  def nextPosition(b: Board): Position
}
