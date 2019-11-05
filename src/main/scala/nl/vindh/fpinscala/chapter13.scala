package nl.vindh.fpinscala

import nl.vindh.fpinscala.chapter11.Monad

object chapter13 extends App {
  sealed trait Free[F[_], A] {
    // Exercise 13.1
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen(Return(_)))

    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)


  }

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F,A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  // Exercise 13.2
  // NOTE: for some reason the presentation compiler does not handle the types correctly
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(s, k) => s match {
      case Return(a) => runTrampoline(k(a))
      case Suspend(r) => runTrampoline(k(r()))
      case FlatMap(s, kk) => runTrampoline(s flatMap(a => kk(a) flatMap k))
    }
  }


}

object Test {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}
