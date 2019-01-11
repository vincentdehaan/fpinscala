package nl.vindh.evaluator



object Test extends App {
  import Evaluator._

  /*println(defining{
    val y = 4
    def f(n: Int, m: Int): Int = n + m
    evaluate((2 + f(5, 7)) * f(1, f(3, 4)))
  })*/

  defining {
    def init[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Nil has no init")
      case x :: Nil => Nil
      case x :: xs => x :: init(xs)
    }

    val xs = List(1, 2, 3, 4, 5)
    evaluate{
      init(xs)
    }
  }
}
