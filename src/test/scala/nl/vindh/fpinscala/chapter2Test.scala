package nl.vindh.fpinscala

import org.scalatest.{FlatSpec, Matchers}
import chapter2._

class chapter2Test extends FlatSpec with Matchers {
  "fib" should "return Fibonnaci numbers" in {
    // Arrange

    // Act
    val fib5 = fib(5)
    val fib10 = fib(10)

    // Assert
    assert(fib5 === 3)
    assert(fib10 === 34)
  }

  "isSorted" should "determine whether a list is sorted" in {
    // Arrange
    val sortedArray = Array(1, 2, 3, 4, 5)
    val almostSortedArray = Array(1, 2, 3, 5, 4)
    val compare: (Int, Int) => Boolean = _ < _

    // Act
    val sorted = isSorted(sortedArray, compare)
    val almostSorted = isSorted(almostSortedArray, compare)

    // Assert
    assert(sorted)
    assert(!almostSorted)
  }

  "curry" should "respect the function values" in {
    // Arrange
    val f: (Int, Int) => Int = (x, y) => x * y + x + y

    // Act
    val fcurry = curry(f)

    // Assert
    assert(f(3, 4) === fcurry(3)(4))
    assert(f(234, 456) === fcurry(234)(456))
  }

  "uncurry" should "respect the function values" in {
    // Arrange
    val f: (Int, Int) => Int = (x, y) => x * y + x + y
    val fcurry = curry(f)

    // Act
    val funcurry = uncurry(fcurry)

    // Assert
    assert(f(3, 4) === funcurry(3, 4))
    assert(f(234, 456) === funcurry(234, 456))
  }

  "compose" should "respect the function values" in {
    // Arrange
    val f: Int => Int = x => x + 7
    val g: Int => Int = x => 3 * x

    // Act
    val fcompg = compose(f, g)

    // Assert
    assert(fcompg(3) === 16)
    assert(fcompg(12) === 43)
  }


}
