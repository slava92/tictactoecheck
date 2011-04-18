package tictactoe

import fj.F

class FixedPoint[A,B] {
  //fix :: (a -> a) -> a
  //fix f = let x = f x in x
  type T = A => B
  val fix: (T=>T)=>T = { f: (T=>T) =>
    lazy val rec: T = f(rec)(_); f(rec)(_)
  }
}

object FixedPoint {
  def main(args: Array[String]): Unit = {
    val fp = new FixedPoint[Int,String]
    type T = fp.T // Int=>String
    val factorial: T=>T = { r: T => { n: Int =>
        if (n <= 1) "1" else (n * r(n-1).toInt).toString
      }
    }
    val fc5 = fp.fix(factorial)(5)
    printf("f5=%s\n", fc5)
  }

  def fjF[B,T](r: B => T): F[B,T] = {
    new F[B,T]() { def f(b: B) = r(b) }
  }

  def printBoard(b: BoardLike) {
    print(b.toString(ttt.Main.simpleCharsF))
  }
}
