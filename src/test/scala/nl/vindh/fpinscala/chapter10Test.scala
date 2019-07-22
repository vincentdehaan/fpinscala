package nl.vindh.fpinscala

import org.scalatest.{FlatSpec, Matchers}
import nl.vindh.fpinscala.chapter10._

class chapter10Test extends FlatSpec with Matchers {
  "isSorted" should "determine if a list is sorted" in {
    // Arrange
    val sorted = IndexedSeq(1, 2, 3, 4, 5)
    val notSorted = IndexedSeq(1, 2, 4, 3, 5)
    val empty = IndexedSeq[Int]()

    // Act
    val sortedS = isSorted(sorted)
    val notSortedS = isSorted(notSorted)
    val emptyS = isSorted(empty)

    // Assert
    assert(sortedS === true)
    assert(notSortedS === false)
    assert(emptyS === true)
  }

  "countWords" should "count the words in a string" in {
    // Arrange
    val w1 = "word"
    val w4 = "word and another word"

    // Act
    val w1c = countWords(w1)
    val w4c = countWords(w4)

    // Assert
    assert(w1c === 1)
    assert(w4c === 4)
  }
}
