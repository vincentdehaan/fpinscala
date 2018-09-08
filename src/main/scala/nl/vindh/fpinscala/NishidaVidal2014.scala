package nl.vindh.fpinscala

// This class implements some ideas from Nishida & Vidal (2014). Conversion to tail recursion in term rewriting, The Journal of Logic and Algebraic Programming 83 (2014), 53-63.
object NishidaVidal2014 {
  sealed trait PeanoNumber {
    def toInt: Int = this match {
      case Z => 0
      case S(n) => n.toInt + 1
    }
    override def toString: String = toInt.toString
  }
  case object Z extends PeanoNumber
  case class S(prev: PeanoNumber) extends PeanoNumber

  // Example 2.
  def fib(n: PeanoNumber): PeanoNumber = n match {
    case Z => Z
    case S(Z) => S(Z)
    case S(S(n)) => add(fib(S(n)), fib(n))
  }

  def add(n: PeanoNumber, m: PeanoNumber): PeanoNumber = (n, m) match {
    case (Z, y) => y
    case (S(x), y) => S(add(x, y))
  }

  // Tail recursive rewriting
  def fib1(n: PeanoNumber): PeanoNumber = fibtail(n, Id)

  sealed trait Context
  case object Id extends Context
  case class Cont(k: Context, x: PeanoNumber) extends Context // What is the role of k?

  @annotation.tailrec
  def fibtail(n: PeanoNumber, c: Context): PeanoNumber = (n, c) match {
    case (Z, k) => eval(k, Z)
    case (S(Z), k) => eval(k, S(Z)) // There seems to be a typo in the paper
    case (S(S(n)), k) => fibtail(S(n), Cont(k, n))
  }

  def eval(c: Context, x: PeanoNumber): PeanoNumber =(c, x) match {
    case (Id, x) => x
    case (Cont(k, x), w) => eval(k, add(w, fib1(x))) // There seems to be a typo in the paper
  }

  def main(args: Array[String]): Unit = {
    val f = fib1(S(S(S(S(S(S(S(Z)))))))) // fib(7) = 13
    println(f)
  }
}
