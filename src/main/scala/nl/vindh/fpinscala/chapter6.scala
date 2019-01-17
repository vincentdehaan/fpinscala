package nl.vindh.fpinscala

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
    (0 until count).foldLeft((List[Int](), rng)){
      (tup, _) => {
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
}
