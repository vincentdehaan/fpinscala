package nl.vindh.fpinscala

import nl.vindh.fpinscala.chapter11.Monad

import scala.collection.immutable.{List => ScalaList}
import scala.{Either => ScalaEither, Right => ScalaRight}

object chapter12 {
  import chapter11.Functor

  trait Applicative[F[_]] extends Functor[F] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: ScalaList[A])(f: A => F[B]): F[ScalaList[B]] =
      as.foldRight(unit(ScalaList[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    // Exercise 12.1
    def sequence[A](fas: ScalaList[F[A]]): F[ScalaList[A]] =
      fas.foldRight(unit(ScalaList[A]()))((nw, acc) => map2(nw, acc)(_ :: _))

    def replicateM[A](n: Int, fa: F[A]): F[ScalaList[A]] =
      sequence(ScalaList.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))

    // Exercise 12.2
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((f, x) => f(x))

    def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply[(A, B), C](unit((t: (A, B)) => f(t._1, t._2)))(product(fa, fb))

    // Exercise 12.3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    // Exercise 12.8
    /*def product[G[_]](G: Applicative[G]) = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      }
    }*/

    // Exercise 12.9
    def compose[G[_]](G: Applicative[G]) = {
      val self = this
      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
          val g: (G[A], G[B]) => G[C] = (ga, gb) => G.map2(ga, gb)(f)
          self.map2(fa, fb)(g)
        }
      }
    }

    // Exercise 12.12
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      ofa.foldRight(unit(Map[K, V]())){
        case ((nwK, nwFV), accF) => map2(nwFV, accF)((nwV, acc) => acc + (nwK -> nwV))
      }


    trait Traverse[F[_]] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        sequence(map(fa)(f))
      def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
        traverse(fga)(ga => ga)
    }
  }

  // Exercise 12.5
  def eitherMonad[E] = new Monad[({type f[x] = ScalaEither[E, x]})#f] {
    def flatMap[A, B](ma: ScalaEither[E, A])(f: A => ScalaEither[E, B]): ScalaEither[E, B] =
      ma.flatMap(f)

    def unit[A](a: => A): ScalaEither[E, A] = ScalaRight(a)
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  // Exercise 12.6
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(_), Failure(hd, tl)) => Failure(hd, tl)
        case (Failure(hd, tl), Success(_)) => Failure(hd, tl)
        case (Failure(hda, tla), Failure(hdb, tlb)) => Failure(hda, tlb ++ tla ++ Vector(hdb))
      }
  }
}
