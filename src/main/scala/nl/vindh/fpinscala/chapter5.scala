package nl.vindh.fpinscala

sealed trait Stream[+A] {
  def nthElt(n: Int): A = this match {
    case Empty => ???
    case LCons(hd, tl) => if(n == 0) hd() else tl().nthElt(n - 1)
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case LCons(hd, tl) => Cons(hd(), tl().toList)
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case LCons(hd, tl) => if(n > 1) LCons(hd, () => tl().take(n - 1)) else LCons(hd, () => Stream.empty)
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case LCons(hd, tl) => if(n > 1) tl().drop(n - 1) else tl()
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case LCons(hd, tl) => if(p(hd())) LCons(hd, () => tl().takeWhile(p)) else Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case LCons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) Stream.cons(a, b) else Stream.empty)

  // Exercise 5.6
  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if(f(a)) Stream.cons(a, b) else b)

  def append[B >: A](x: => B): Stream[B] = foldRight(Stream.cons(x, Empty: Stream[B]))((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).foldRight(b)((c, d) => Stream.cons(c, d)))

  // Exercise 5.13
  // TODO: test!
  def mapUf[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case LCons(h, t) => Stream.unfold(t){
      s => s() match {
        case Empty => None
        case LCons(hd, tl) => Some((f(hd()), tl))
      }
    }
  }

  /*def takeUf(n: Int): Stream[A] = this match {
    case Empty => Empty
    case LCons(h, t) => Stream.unfold(n - 1)(s => if(s > 0) )
  }*/

  def takeWhileUf(p: A => Boolean): Stream[A] = ???

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = ???

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

}
case object Empty extends Stream[Nothing]
case class LCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] // I call this LCons to prevent confusion with chapter 3

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    LCons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val xs: Stream[A] = Stream.cons(a, xs)
    xs
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def fibsRec(nMinOne: Int, nMinTwo: Int): Stream[Int] = Stream.cons(nMinOne + nMinTwo, fibsRec(nMinOne + nMinTwo, nMinOne))
    Stream.cons(1, fibsRec(1, 0))
  }

  // Exercise 5.11
  // This function will be tested in exercise 5.12
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  // Exercise 5.12
  def fibsUf: Stream[Int] = Stream.cons(1, unfold((1, 0))(s => Some((s._1 + s._2, (s._1 + s._2, s._1)))))

  def fromUf(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constantUf[A](a: A): Stream[A] = unfold(())(x => Some((a, ())))

  def onesUf: Stream[Int] = constantUf(1)
}

object chapter5 extends App {
  val s = Stream(1, 2, 3, 4)
  s.mapUf(x => {println(123); x + 1})
  println(s.toList)
}
