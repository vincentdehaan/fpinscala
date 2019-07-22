package nl.vindh.fpinscala

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.{List => ScalaList}

class chapter11Test extends FlatSpec with Matchers {
  import chapter11._
  "filterM" should "filter a list with a monadic filter function" in {
    // Arrange
    val l = ScalaList(1, 2, 3)
    def filter1(i: Int): Option[Boolean] = i match {
      case 1 => Some(true)
      case 2 => Some(false)
      case 3 => None
    }
    def filter2(i: Int): Option[Boolean] = i match {
      case 1 => Some(false)
      case _ => Some(true)
    }

    // Act
    val f1 = optionMonad.filterM(l)(filter1)
    val f2 = optionMonad.filterM(l)(filter2)

    // Assert
    assert(f1 === None)
    assert(f2 === Some(ScalaList(2, 3)))
  }

  "flatMapViaCompose" should "act like flatMap" in {
    // Arrange
    val o = Some(1)
    def f(i: Int): Option[Int] = Some(i + 1)

    // Act
    val res = optionMonad.flatMapViaCompose(o)(f)

    // Assert
    assert(res === Some(2))
  }
}
