package tictactoe

import org.scalacheck._
import PlayerCheck.checkProp
import PlayerCheck.players

object GameResultCheck {
  val gameResults = Gen.choose(0,2).map {n => GameResult.values.apply(n)}
  val propWin = Prop.forAll(players) { p: Player =>
    GameResult.win(p).winner.some == p
  }
  val propFold = Prop.forAll(gameResults) { g =>
    val r = g.strictFold(1, 2, 0)
    g match {
      case GameResult.Player1Wins => 1 == r
      case GameResult.Player2Wins => 2 == r
      case GameResult.Draw =>        0 == r
    }
  }
  val propFromStr = Prop.forAll(gameResults) { g =>
    GameResult.valueOf(g.toString) == g
  }

  def main(args: Array[String]): Unit = {
    checkProp(propWin)
    checkProp(propFold)
    checkProp(propFromStr)
  }

}
