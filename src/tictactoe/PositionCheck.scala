package tictactoe

import org.scalacheck._
import PlayerCheck.checkProp

object PositionCheck {
  val positions = Gen.choose(1,9).map {n => Position.fromInt(n).some}
  val propFromChar = Prop.forAll(positions) { p =>
    Position.fromChar(p.toChar).some == p
  }
  val propFromInt = Prop.forAll(positions) { p =>
    Position.fromInt(p.toInt).some == p
  }
  val propFromStr = Prop.forAll(positions) { p =>
    Position.valueOf(p.toString) == p
  }
  val propPositions = Prop.forAll(positions) { p =>
    Position.positions.length == Position.values.length
  }

  def main(args: Array[String]): Unit = {
    checkProp(propFromChar)
    checkProp(propFromInt)
    checkProp(propFromStr)
    checkProp(propPositions)
  }

}
