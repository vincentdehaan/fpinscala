package nl.vindh.fpinscala

import org.scalatest.{FlatSpec, Matchers}
import chapter6._

class chapter6Test extends FlatSpec with Matchers {
  case class FakeRNG(constant: Int) extends RNG {
    def nextInt: (Int, RNG) = (constant, this)
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
    val rng = SimpleRNG(42)

    // Act
    val (is, _) = ints(7)(rng)

    // Assert
    assert(List.length(is) === 7)
    // We assume that all elements of a small list of random integers are distinct and non-zero
    val distinct = List.foldRight(is, (true, 0))((next, tup) => ((next != tup._2) && tup._1, next))._1
    assert(distinct === true)
  }

  "doubleWithMap" should "generate a random double between 0 and 1" in {
    // Arrange
    val maxRng = FakeRNG(Int.MaxValue)

    // Act
    val (d, _) = doubleWithMap(maxRng)

    // Assert
    assert(d >= 0 && d < 1)
  }
}
