package nl.vindh.fpinscala

object chapter7 extends App {






  object Par {
    def unit[A](a: => A): Par[A] = ???
    def get[A](a: Par[A]): A = ???

    // Exercise 7.1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

//    def
  }

  // Exercise 7.2
  // case class Par[A](get: () => A)



}
