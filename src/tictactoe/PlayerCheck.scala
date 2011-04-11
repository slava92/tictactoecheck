package tictactoe

import org.scalacheck._

object PlayerCheck {
  val players = Gen.choose(0,1).map {n => Player.values.apply(n)}
  val propAlternate = Prop.forAll(players) { p =>
    p.alternate.alternate == p
  }
  val propToStr = Prop.forAll(players) { p =>
    Player.valueOf(p.toString) == p
  }
  def checkProp(prop: Prop) {
    Test.check(Test.Params(testCallback = ConsoleReporter(1)), prop)
  }
  def main(args: Array[String]): Unit = {
    checkProp(propAlternate)
    checkProp(propToStr)
  }
}
