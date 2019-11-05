package nl.vindh.fpinscala

import scala.collection.immutable.{List => ScalaList}
import scala.util.Try

object chapter14 extends App {
  sealed trait ST[S, A] {
    self =>
    protected def run(s: S): (A, S)
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }
    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S) = (memo, s)
      }
    }

    def runST[A](st: RunnableST[A]): A =
      st.apply[Unit].run(())._1
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, ScalaList[A]] = ST(value.toList)

    // Exercise 14.1
    def fill(xs: Map[Int, A]): ST[S, Unit] =
      xs.foldRight(ST[S, Unit](())) {
        case ((k, v), st) => st flatMap (_ => write(k, v))
      }

    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
      ST(new STArray[S,A] {
        lazy val value = Array.fill(sz)(v)
      })

    def fromList[S, A: Manifest](xs: ScalaList[A]): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = xs.toArray
      })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  // Exercise 14.1 (example)
  Try {
    val filledST = new RunnableST[ScalaList[String]] {
      def apply[S] = for {
        arr <- STArray(5, "a")
        _ <- arr.fill(Map(1 -> "c", 7 -> "d")) // NOTE: this throws a very ugly error as there is no bound check
        arrv <- arr.freeze
      } yield arrv
    }

    val filled = ST.runST(filledST)
    println(filled)
  }
}
