package nl.vindh.fpinscala

import fpinscala.testing.{Gen, Prop}
import scala.collection.immutable.{List => ScalaList}

object chapter10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1 match {
        case Some(x) => Some(x)
        case None => a2
      }

    def zero: Option[A] = None
  }

  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def zero: A => A = x => x
  }

  // Exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(Gen.listOfN(3, gen)) {
      case ScalaList(a, b, c) =>
        m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) &&
          m.op(a, m.zero) == a &&
          m.op(m.zero, a) == a
    }

  // Exercise 10.5
  def foldMap[A, B](as: ScalaList[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)
  // NOTE: This could be done without map!

  // Exercise 10.6
  // I could not solve this one!

  // Exercise 10.7ot solve this one!

  // Exercis
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if(v.isEmpty) m.zero
    else if(v.length == 1) f(v.head)
    else {
      val (l, r) = v.splitAt((v.length / 2).floor.toInt)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // Exercise 10.9
  val sortMonoid = new Monoid[(Int, Int, Boolean)] {
    def op(a: (Int, Int, Boolean), b: (Int, Int, Boolean)): (Int, Int, Boolean) = // (min, max, sorted)
      (a._1 min b._1, a._2 max b._2, a._3 && b._3 && a._2 < b._1)
    def zero: (Int, Int, Boolean) = (Int.MaxValue, Int.MinValue, true)
  }

  def isSorted(is: IndexedSeq[Int]): Boolean = foldMap(is.toList, sortMonoid)((i: Int) => (i, i, true))._3

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  // NOTE: I did not solve 10.10 and 10.11 correctly by myself.
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero = Stub("")
    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  // Exercise 10.11
  def countWords(str: String): Int = {
    def wc(c: Char): WC =
      if(c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(str.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
    // Exercise 10.15
    def toList[A](as: F[A]): ScalaList[A] =
      foldRight(as)(ScalaList[A]())((nw, acc) => nw :: acc)
  }

  // Exercise 10.12
  val foldableIndexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  // Exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def zero: (A, B) = (A.zero, B.zero)
      def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    }

  // Exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero: A => B = _ => B.zero
      def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) {
          (acc, k) => acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }
  // Exercise 10.18

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m = mapMergeMonoid[A, Int](intAddition)
    as.map(a => Map(a -> 1)).foldLeft(m.zero)(m.op)
  }
}
