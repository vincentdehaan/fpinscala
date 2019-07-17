package nl.vindh.fpinscala

import scala.collection.immutable.{List => ScalaList}

object chapter8 {
  //trait Prop {
    //type FailedCase = String
    //type SuccessCount = Int
    //def check: Either[(FailedCase, SuccessCount), SuccessCount]

    // Exercise 8.3
    //def &&(that: Prop): Prop = new Prop {
    //  def check: Boolean = this.check && that.check
    //}
  //}

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
  case class Gen[A](sample: State[RNG, A]){
    // Exercise 8.6a
    // NOTE: This could be done more nicely if I had implemented flatMap on State
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      val a = (r: RNG) => sample.run(r)._1
      val b = a.andThen(f)
      val c = (r: RNG) => b(r).sample.run(r)
      Gen(State(c))
    }

    def listOfN(size: Gen[Int]): Gen[ScalaList[A]] =
      size.flatMap(n => Gen.listOfN(n, this))


    // Exercise 9.10
    def unsized: SGen[A] = SGen(i => this)
  }

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

    // Exercise 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      Gen.boolean.flatMap(b => if(b) g1 else g2)

    // Exercise 8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      {
        val total = g1._2 + g2._2
        val g1w = (g1._2 / total * 100).toInt
        choose(0, 100).flatMap(n => if(n < g1w) g1._1 else g2._1)
      }
  }

  case class SGen[A](forSize: Int => Gen[A])

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case class Prop(run: (TestCases, RNG) => Result){
    // Exercise 8.9
    def &&(p: Prop): Prop =
      Prop((t, r) => {
        val thisRun = this.run(t, r)
        val pRun = p.run(t, r)
        (thisRun, pRun) match {
          case (Passed, Passed) => Passed
          case (Passed, f: Falsified) => f
          case (f: Falsified, Passed) => f
            // NOTE: choosing the minimum of the number of successes is not necessarily correct
          case (f1: Falsified, f2: Falsified) => Falsified(f1.failure + ", " + f2.failure, f1.successes.min(f2.successes))
        }
      })
  }
}
