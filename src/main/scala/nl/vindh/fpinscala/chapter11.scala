package nl.vindh.fpinscala

import scala.collection.immutable.{List => ScalaList}

object chapter11 {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    // Exercise 11.3
    def sequence[A](lma: ScalaList[F[A]]): F[ScalaList[A]] =
      lma.foldRight(unit(ScalaList[A]()))((nw, acc) => map2(nw, acc)(_ :: _))

    def traverse[A, B](la: ScalaList[A])(f: A => F[B]): F[ScalaList[B]] =
      sequence(la.map(f))

    // Exercise 11.4
    def replicateM[A](n: Int, ma: F[A]): F[ScalaList[A]] =
      sequence(ScalaList.fill(n)(ma))

    // Exercise 11.6
    def filterM[A](ms: ScalaList[A])(f: A => F[Boolean]): F[ScalaList[A]] =
      ms.foldRight(unit(ScalaList[A]())) {
        (nw, accF) => map2(f(nw), accF) {
          (b, acc) => if(b) nw :: acc else acc
        }
      }

    // Exercise 11.7
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    // Exercise 11.8
    def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
      compose((_: Unit.type) => ma, f)(Unit)

    // Exercise 11.12
    def join[A](mma: F[F[A]]): F[A] =
      flatMap(mma)(x => x)

    // Exercise 1.13
    def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(ma)(f))
  }

  // Exercise 11.1
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  // Exercise 11.2
  import fpinscala.state.State
  def stateMonad[S] = {
    type StateS[A] = State[S, A]
    new Monad[StateS] {
      def unit[A](a: => A): StateS[A] = State(s => (a, s))
      def flatMap[A, B](ma: StateS[A])(f: A => StateS[B]): StateS[B] =
        ma.flatMap(f)
    }
  }

  // Exercise 11.17
  case class Id[A](value: A){
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad = new Monad[Id] {
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
    def unit[A](a: => A): Id[A] = Id(a)
  }

  // Exercise 11.20
  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(r => a)
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(
          r => {
            val a = st.run(r)
            f(a).run(r)
          }
        )
    }
  }
}
