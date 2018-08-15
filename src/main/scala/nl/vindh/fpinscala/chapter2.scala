package nl.vindh.fpinscala

object chapter2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(a: Int, b: Int, count: Int): Int = {
      if(count == 3) a + b
      else loop(b, a + b, count - 1)
    }
    if(n == 1) 0
    else if (n == 2) 1
    else loop(0, 1, n)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(pos: Int): Boolean = {
      if(as.length == pos + 1) true
      else if(!ordered(as(pos), as(pos + 1))) false
      else loop(pos + 1)
    }

    loop(0)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))


  def main(args: Array[String]): Unit = {
    (1 to 10).foreach{
      n => println(s"The ${n}th element of the Fibonacci series is: ${fib(n)}.");
    }


  }
}
