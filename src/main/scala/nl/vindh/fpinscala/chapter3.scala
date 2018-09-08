package nl.vindh.fpinscala

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  // The answer is 1 + 2 = 3

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

  // Exercise 3.7
  // No. Application of the operator takes place after traversing the whole list.

  // Exercise 3.8
  // This results in the list itself.

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  // Exercise 3.10
  // This exercise was difficult because the book did not specify that fold started at f(z, x1) instead of f(z, xn)
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(acc: B, lst: List[A]): B =
      lst match {
        case Nil => acc
        case Cons(x, xs) => loop(f(acc, x), xs)
      }

    loop(z, as)
  }

  // Exercise 3.11
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => 1 + x)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil.asInstanceOf[List[A]])((tl, hd) => Cons(hd, tl))

  // Exercise 3.13
  def reverseNoFold[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def loop(lst: List[A], rev: List[A]): List[A] = lst match {
      case Nil => rev
      case Cons(x, xs) => loop(xs, Cons(x, rev))
    }

    loop(as, Nil)
  }

  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val rev = reverseNoFold(as)
    foldRight(rev, z)((a, b) => f(b, a))
  }

  def foldRightL[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val rev = reverseNoFold(as)
    foldLeft(rev, z)(f)
  }

  // Exercise 3.14
  // I assume that append means append at the end, i.e. append(Cons(1, Cons(2, Nil)), 3) = Cons(1, Cons(2, Cons(3, Nil)))
  def append[A](as: List[A], z: A): List[A] = foldRight(as, Cons(z, Nil))(Cons(_, _))

  // Exercise 3.15
  def flatten[A](lsts: List[List[A]]): List[A] =
    foldLeft(lsts, Nil.asInstanceOf[List[A]]){
      (lst1, lst2) => foldLeft(lst2, lst1)((lst, elt) => append(lst, elt))
    }

  // Exercise 3.16
  def addOne(is: List[Int]): List[Int] = is match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  // Exercise 3.17
  def doubleToString(ds: List[Double]): List[String] = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  // Exercise 3.18
  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Exercise 3.19
  // See the test case for the application of this method
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  // Exercise 3.20
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = flatten(map(xs)(f))

  // Exercise 3.21
  def filterFM[A](xs: List[A])(f: A => Boolean): List[A] = flatMap(xs){
    x => if(f(x)) Cons(x, Nil) else Nil
  }

  // Exercise 3.22
  def add(xlst: List[Int], ylst: List[Int]): List[Int] = (xlst, ylst) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
    case _ => throw new Exception("Lists not of equal length.")
  }

  // Exercise 3.23
  def zipWith[A, B, C](xlst: List[A], ylst: List[B])(f: (A, B) => C): List[C] = (xlst, ylst) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => throw new Exception("Lists not of equal length.")
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(x, xs), Cons(y, ys)) => (x == y && hasSubsequence(xs, ys)) || hasSubsequence(xs, sub)
    case (Nil, _ ) => false
    case (_, Nil) => true
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // TODO: testall
  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A], z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(x) => z(x)
    case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
  }

  def sizeF[A](t: Tree[A]): Int = fold(t, (_: A) => 1)(_ + _ + 1)

  def maximumF(t: Tree[Int]): Int = fold(t, (x: Int) => x)(_ max _)

  def depthF[A](t: Tree[A]): Int = fold(t, (_: A) => 1)((x: Int, y: Int) => (x max y) + 1)

  def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    val z : A => Tree[B] = x => Leaf(f(x)) // We need this to satisfy the type checker
    fold(t, z)((l: Tree[B], r: Tree[B]) => Branch(l, r))
  }
}
