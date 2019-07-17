package nl.vindh.fpinscala

import scala.collection.immutable.{List => ScalaList}

object chapter8 {
  trait Prop {
    type FailedCase = String
    type SuccessCount = Int
    def check: Either[(FailedCase, SuccessCount), SuccessCount]

    // Exercise 8.3
    //def &&(that: Prop): Prop = new Prop {
    //  def check: Boolean = this.check && that.check
    //}


  }

  case class State[S, A](run: S => (A, S)){
    def map[A, B](f: A => B): State[S, B] =
      State {
        s => {
          val (aa, ss) = run(s)
          val b = f(aa.asInstanceOf[A])
          (b, ss)
        }
      }
  }
  case class Gen[A](sample: State[RNG, A])

  object Gen{
    // Exercise 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(chapter6.nonNegativeInt).map((n: Int) => n % (stopExclusive - start)))

    // Exercise 8.5
    def unit[A](a: => A): Gen[A] =
      Gen(State(chapter6.nonNegativeInt).map((_: Any) => a))

    def boolean: Gen[Boolean] =
      Gen(State(chapter6.nonNegativeInt).map((_: Int) % 2 == 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[ScalaList[A]] =
      Gen(State(chapter6.sequence(ScalaList.fill(n)(g.sample.run))))
  }
}
