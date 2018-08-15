package nl.vindh.fpinscala

import org.scalatest.{FlatSpec, Matchers}

class chapter3Test extends FlatSpec with Matchers{
  "tail" should "return all but the first element" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val t = List.tail(l)

    // Assert
    assert(t === List(2, 3, 4))
  }

  "setHead" should "return a List with another first element" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val ll = List.setHead(l, 7)

    // Assert
    assert(ll === List(7, 2, 3, 4))
  }

  "drop" should "return a List without the first n elements" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val ll = List.drop(l, 2)

    // Assert
    assert(ll === List(3, 4))
  }

  "dropWhile" should "drop elements as long as a condition is satisfied" in {
    // Arrange
    val l = List(1, 2, 3, 4)
    val f = (x: Int) => x < 3

    // Act
    val ll = List.dropWhile(l, f)

    // Assert
    assert(ll === List(3, 4))
  }

  "init" should "return the List without the last element" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val ll = List.init(l)

    // Assert
    assert(ll == List(1, 2, 3))
  }

  "length" should "return the length of the List" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val len = List.length(l)

    // Assert
    assert(len === 4)
  }
}
