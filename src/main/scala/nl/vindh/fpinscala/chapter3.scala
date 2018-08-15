package nl.vindh.fpinscala

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Nil has no tail!")
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], x: A): List[A] = Cons(x, tail(l))

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if(n == 0) l
    else if(n == 1) tail(l)
    else drop(tail(l), n - 1)

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Nil has no init")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

}

object chapter3 {
  // Exercise 3.1
  // The answer is 1 + 2 = 3

  // Exercise 3.7
  // No. Application of the operator takes place after traversing the whole list.

  // Exercise 3.8
  // This results in the list itself.

}
