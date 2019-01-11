package nl.vindh.fpinscala

import scala.collection.immutable.{List => ScalaList}

import org.scalatest.{Assertion, FlatSpec, Matchers}

class chapter4Test extends FlatSpec with Matchers {
  "Option" should "support map" in {
    // Arrange
    val s = Some(23)
    val n = None
    val f = (x: Int) => x + 1

    // Act
    val sf = s map f
    val nf = n map f

    // Assert
    assert(sf === Some(24))
    assert(nf === None)
  }

  it should "support flatMap" in {
    // Arrange
    val s = Some(23)
    val n = None
    val f = (x: Int) => Some(x + 1)

    // Act
    val sf = s flatMap f
    val nf = n flatMap f

    // Assert
    assert(sf === Some(24))
    assert(nf === None)
  }

  it should "getOrElse" in {
    // Arrange
    val s = Some(23)
    val n = None

    // Act
    val sOrElse = s.getOrElse(10)
    val nOrElse = n.getOrElse(11)

    // Assert
    assert(sOrElse === 23)
    assert(nOrElse === 11)
  }

  it should "support orElse" in {
    // Arrange
    val s = Some(23)
    val n = None

    // Act
    val sOrElse = s.orElse(Some(10))
    val nOrElse = n.orElse(Some(11))

    // Assert
    assert(sOrElse === Some(23))
    assert(nOrElse === Some(11))
  }

  it should "support filter" in {
    // Arrange
    val s = Some(23)
    val f1: Int => Boolean = _ > 10
    val f2: Int => Boolean = _ < 10

    // Act
    val filter1 = s.filter(f1)
    val filter2 = s.filter(f2)

    // Assert
    assert(filter1 === Some(23))
    assert(filter2 === None)
  }

  "mean and variance" should "calculate the mean and variance of a non-empty list of integers" in {
    // Arrange
    val is = ScalaList(1.0, 2.0, 3.0, 4.0, 5.0)

    // Act
    val m = Option.mean(is)
    val v = Option.variance(is)

    // Assert
    assert(m === Some(3.0))
    assert(v === Some(2.0))
  }

  it should "return None for the empty list" in {
    // Arrange
    val is = ScalaList()

    // Act
    val m = Option.mean(is)
    val v = Option.variance(is)

    // Assert
    assert(m === None)
    assert(v === None)
  }

  "map2" should "map a function of two variables on two Options" in {
    // Arrange
    val f: (Int, Int) => Int = _ + _ + 1
    val o = Some(3)
    val p = Some(4)

    // Act
    val m = Option.map2(o, p)(f)

    // Assert
    assert(m === Some(8))
  }

  def sequenceTest(sequenceImpl: List[Option[Int]] => Option[List[Int]]): Assertion = {
    // Arrange
    val s = List(Some(1), Some(2), Some(3))
    val n = List(Some(1), Some(2), None)

    // Act
    val ss = Option.sequence(s)
    val nn = Option.sequence(n)

    // Assert
    assert(ss === Some(List(1, 2, 3)))
    assert(nn === None)
  }

  "sequence" should "convert a sequence of Options to an Option of a sequence" in {
    sequenceTest(Option.sequence)
  }

  "traverse" should "traverse a list and apply a function that returns an Option" in {
    // Arrange
    val is = List(1, 2, 3, 4)
    val fn: Int => Option[Int] = x => if(x < 2) None else Some(x)
    val fs: Int => Option[Int] = x => Some(x + 1)

    // Act
    val n = Option.traverse(is)(fn)
    val s = Option.traverse(is)(fs)

    // Assert
    assert(n === None)
    assert(s === Some(List(2, 3, 4, 5)))
  }

  it should "be usable to implement sequence" in {
    sequenceTest(Option.travSequence)
  }

  "Either" should "support map" in {
    // Arrange
    val l = Left(23)
    val r = Right(4)
    val f = (x: Int) => x + 1

    // Act
    val lf = l map f
    val rf = r map f

    // Assert
    assert(lf === Left(23))
    assert(rf === Right(5))
  }

  it should "support flatMap" in {
    // Arrange
    val l = Left(23)
    val r = Right(4)
    val f = (x: Int) => Right(x + 1)

    // Act
    val lf = l flatMap f
    val rf = r flatMap f

    // Assert
    assert(lf === Left(23))
    assert(rf === Right(5))
  }

  it should "support orElse" in {
    // Arrange
    val l = Left(23)
    val r = Right(5)
    val d = Right(6)

    // Act
    val lOrElse = l.orElse(d)
    val rOrElse = r.orElse(d)

    // Assert
    assert(lOrElse === Right(6))
    assert(rOrElse === Right(5))
  }

  it should "support map2" in {
    // Arrange
    val ra = Right(12)
    val rb = Right(7)
    val f: (Int, Int) => Int = _ + _ + 2

    // Act
    val m = ra.map2(rb)(f)

    // Assert
    assert(m === Right(21))
  }

  it should "support map2 implemented with a for comprehension" in {
    // Arrange
    val ra = Right(12)
    val rb = Right(7)
    val f: (Int, Int) => Int = _ + _ + 2

    // Act
    val m = ra.map2for(rb)(f)

    // Assert
    assert(m === Right(21))
  }

  "traverse (Either)" should "traverse a list and apply a function that returns an Either" in {
    // Arrange
    val is = List(1, 2, 3, 4)
    val fl: Int => Either[String, Int] = x => if(x < 2) Left("Small") else Right(x)
    val fr: Int => Either[String, Int] = x => Right(x + 1)

    // Act
    val l = Either.traverse(is)(fl)
    val r = Either.traverse(is)(fr)

    // Assert
    assert(l === Left("Small"))
    assert(r === Right(List(2, 3, 4, 5)))
  }

  "sequence (Either)" should "convert a sequence of Eithers to an Either of a sequence" in {
    // Arrange
    val r = List(Right(1), Right(2), Right(3))
    val l = List(Right(1), Right(2), Left("No!"))

    // Act
    val rr = Either.sequence(r)
    val ll = Either.sequence(l)

    // Assert
    assert(rr === Right(List(1, 2, 3)))
    assert(ll === Left("No!"))
  }
}
