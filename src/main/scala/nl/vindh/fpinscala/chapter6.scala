package nl.vindh.fpinscala

import scala.collection.immutable.{List => ScalaList}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) + 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >> 16).toInt
    (n, nextRNG)
  }
}

object chapter6 {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, newRng) = rng.nextInt
    (if(i.abs < 0) (i + 1).abs else i.abs, newRng)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, newRng) = nonNegativeInt(rng)
    ((i - 1).toDouble / Int.MaxValue, newRng)
  }

  // Exercise 6.3
  // No testing; I verified the answers based on the Chapter Notes
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldRight((List[Int](), rng)){
      (_, tup) => {
        val (i, nextRng) = tup._2.nextInt
        (Cons(i, tup._1), nextRng)
      }
    }
  }

  // ---
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.5
  def doubleWithMap: Rand[Double] =
    map(nonNegativeInt)((i: Int) => (i - 1).toDouble / Int.MaxValue)

  // Exercise 6.6
  // This will be tested in later excercises
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng)
      (f(a, b), rng3)
    }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => List.foldRight(fs, (List[A](), rng)) {
      (nw, acc) => (Cons(nw(acc._2)._1 , acc._1), nw(acc._2)._2)
    }
  }

  // Necessary in chapter 8
  def sequence[A](fs: ScalaList[Rand[A]]): Rand[ScalaList[A]] = {
    rng => fs.foldRight((ScalaList[A](), rng)) {
      (nw, acc) => (nw(acc._2)._1 :: acc._1, nw(acc._2)._2)
    }
  }

  // This is necessary because we did not implement fill in Chapter 3
  def fillList[A](count: Int, el: A): List[A] = if(count > 0) Cons(el, fillList(count - 1, el)) else Nil

  def intsSeq(count: Int): Rand[List[Int]] = sequence(fillList(count, nonNegativeInt _))

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){
    i => {
      val mod = i % n
      if(i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  // Exercise 6.9
  // Verified against companion booklet
  def mapFm[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2Fm[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapFm(rb)(b => f(a, b)))

  // Exercise 6.10
  case class State[S, +A](run: S => (A, S)){
    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State {
        s => {
          val (a, s2) = run(s)
          g(a).run(s2)
        }
      }

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      State {
        s => List.foldRight(fs, (List[A](), s)) {
          case (nw, (ls, s)) => (Cons(nw.run(s)._1, ls), nw.run(s)._2)
        }
      }

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  // Exercise 6.11
  // Thise one is still too hard for me! I looked at the answer, but I didn't fully understand it.
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
//    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//      for {
//
//        i <- inputs

//      }
  }
}
