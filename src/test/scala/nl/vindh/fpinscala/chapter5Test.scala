package nl.vindh.fpinscala

import org.scalatest.{Assertion, FlatSpec, Matchers}

class chapter5Test extends FlatSpec with Matchers {
  "Stream" should "implement toList" in {
    // Arrange
    val s = Stream(1, 2, 3)

    // Act
    val l = s.toList

    // Assert
    assert(l === List(1, 2, 3))
  }

  it should "implement take" in {
    // Arrange
    val s = Stream(1, 2, 3, 4, 5)

    // Act
    val t = s.take(3)

    // Assert
    assert(t.toList === Stream(1, 2, 3).toList)
  }

  it should "implement drop" in {
    // Arrange
    val s = Stream(1, 2, 3, 4, 5)

    // Act
    val d = s.drop(3)

    // Assert
    assert(d.toList === Stream(4, 5).toList)
  }

  def takeWhileTest(takeWhileImpl: (Stream[Int], Int => Boolean) => Stream[Int]): Assertion = {
    // Arrange
    val s = Stream(1, 2, 3, 4, 5)
    val f: Int => Boolean = _ < 3

    // Act
    val t = takeWhileImpl(s, f)

    // Assert
    assert(t.toList === Stream(1, 2).toList)
  }

  it should "implement takeWhile" in {
    def takeWhileImpl(s: Stream[Int], f: Int => Boolean): Stream[Int] = s.takeWhile(f)
    takeWhileTest(takeWhileImpl)
  }

  it should "implement forAll" in {
    // Arrange
    val st = Stream(1, 2, 3, 4, 5)
    val sf = Stream(-1, -2, 3, 4, 5)
    val g: Int => Boolean = _ > 0

    // Act
    val t = st.forAll(g)
    val f = sf.forAll(g)

    // Assert
    assert(t === true)
    assert(f === false)
  }

  it should "implement takeWhileFR using foldRight" in {
    def takeWhileImpl(s: Stream[Int], f: Int => Boolean): Stream[Int] = s.takeWhileFR(f)
    takeWhileTest(takeWhileImpl)
  }

  it should "implement headOption using foldRight" in {
    // Arrange
    val e = Stream()
    val ne = Stream(1, 2, 3)

    // Act
    val s = ne.headOption
    val n = e.headOption

    // Assert
    assert(s === Some(1))
    assert(n === None)
  }

  def mapTest(mapImpl: (Stream[Int], Int => Int) => Stream[Int]): Assertion = {
    // Arrange
    val s = Stream(1, 2, 3, 4, 5)
    var sideEffect = false
    val f: Int => Int = x => {
      sideEffect = true
      x + 1
    }

    // Act
    val t = mapImpl(s, f)

    // Assert
    assert(sideEffect === false)

    // Act
    t.nthElt(3)

    // Assert
    assert(sideEffect === true)
    assert(t.toList === List(2, 3, 4, 5, 6))
  }

  it should "implement map using foldRight" in {
    def mapImpl(s: Stream[Int], f: Int => Int): Stream[Int] = s.map(f)
    mapTest(mapImpl)
  }

  it should "implement filter using foldRight" in {
    // Arrange
    val s = Stream(1, 2, 3, 4, 5)
    val f: Int => Boolean = x => x % 2 == 0

    // Act
    val t = s.filter(f)

    // Assert
    assert(t.toList === List(2, 4))
  }

  it should "implement append using foldRight and a non-strict argument" in {
    // Arrange
    val s = Stream(1, 2, 3)
    var sideEffect = false

    // Act
    val t = s.append({sideEffect = true; 5})

    // Assert
    assert(sideEffect === false)

    // Act
    t.nthElt(3)

    // Assert
    assert(sideEffect === true)
    assert(t.toList === List(1, 2, 3, 5))
  }

  it should "implement flatMap using foldRight" in {
    // Arrange
    val s = Stream(1, 2, 3)
    val f: Int => Stream[Int] = x => Stream(x, 2 * x)

    // Act
    val t = s.flatMap(f)

    // Assert
    assert(t.toList === List(1, 2, 2, 4, 3, 6))
  }

  def constantTest(constantImpl: Int => Stream[Int]): Assertion = {
    // Act
    val c = constantImpl(7)

    // Assert
    assert(c.take(5).toList === List(7, 7, 7, 7, 7))
  }

  it should "implement constant" in {
    constantTest(Stream.constant)
  }

  def fromTest(fromImpl: Int => Stream[Int]): Assertion = {
    // Act
    val f = fromImpl(7)

    // Assert
    assert(f.take(5).toList === List(7, 8, 9, 10, 11))
  }

  it should "implement from" in {
    fromTest(Stream.from)
  }

  def fibsTest(fibsImpl: Stream[Int]): Assertion = {
    // Act
    val f = fibsImpl

    // Assert
    assert(f.take(6).toList === List(1, 1, 2, 3, 5, 8))
  }

  it should "implement fibs" in {
    fibsTest(Stream.fibs)
  }

  it should "implement constant using unfold" in {
    constantTest(Stream.constantUf)
  }

  it should "implement from using unfold" in {
    fromTest(Stream.fromUf)
  }

  it should "implement fibs using unfold" in {
    fibsTest(Stream.fibsUf)
  }

  it should "implement ones using unfold" in {
    // Act
    val o = Stream.onesUf

    // Assert
    assert(o.take(5).toList === List(1, 1, 1, 1, 1))
  }

  it should "implement map using unfold" in {
    def mapImpl(s: Stream[Int], f: Int => Int): Stream[Int] = s.mapUf(f)
    mapTest(mapImpl)
  }
}
