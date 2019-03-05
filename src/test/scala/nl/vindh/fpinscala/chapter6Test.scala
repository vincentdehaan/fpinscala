package nl.vindh.fpinscala

import org.scalatest.{FlatSpec, Matchers}
import chapter6._

class chapter6Test extends FlatSpec with Matchers {
  case class FakeRNG(constant: Int) extends RNG {
    def nextInt: (Int, RNG) = (constant, this)
  }

  case class PlusOneRng(seed: Int) extends RNG {
    def nextInt: (Int, RNG) = (seed, PlusOneRng(seed + 1))
  }

  "nonNegativeInt" should "generate a random non-negative integer" in {
    // Arrange
    val minRng = FakeRNG(Int.MinValue)
    val maxRng = FakeRNG(Int.MaxValue)

    // Act
    val (i1, _) = nonNegativeInt(minRng)
    val (i2, _) = nonNegativeInt(maxRng)

    // Assert
    assert(i1 >= 0)
    assert(i2 >= 0)
  }

  "double" should "generate a random double between 0 and 1" in {
    // Arrange
    val maxRng = FakeRNG(Int.MaxValue)

    // Act
    val (d, _) = double(maxRng)

    // Assert
    assert(d >= 0 && d < 1)
  }

  "ints" should "generate a list of random integers" in {
    // Arrange
    val poRng = PlusOneRng(1)

    // Act
    val (is, _) = ints(7)(poRng)

    // Assert
    assert(is === List(7, 6, 5, 4, 3, 2, 1))
  }

  "doubleWithMap" should "generate a random double between 0 and 1" in {
    // Arrange
    val maxRng = FakeRNG(Int.MaxValue)

    // Act
    val (d, _) = doubleWithMap(maxRng)

    // Assert
    assert(d >= 0 && d < 1)
  }

  "intsSeq" should "generate a list of random integers" in {
    // Arrange
    val poRng = PlusOneRng(1)

    // Act
    val (is, _) = intsSeq(7)(poRng)

    // Assert
    assert(is === List(7, 6, 5, 4, 3, 2, 1))
  }

  "nonNegativeLessThan" should "generate non-negative integers less than n" in {
    // Arrange
    val poRng = PlusOneRng(1)

    // Act
    val (n, _) = nonNegativeLessThan(7)(poRng)

    // Assert
    assert(n === 1)
  }
}
