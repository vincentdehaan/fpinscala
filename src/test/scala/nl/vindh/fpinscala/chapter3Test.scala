package nl.vindh.fpinscala

import org.scalatest.{Assertion, FlatSpec, Matchers}

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

  "foldRight" should "work properly, even with non-commutative functions" in {
    // Arrange
    val l = List(1, 2, 3, 4)
    val f = (i: Int, s: String) => s"($s->$i)"

    // Act
    val s = List.foldRight(l, "X")(f)

    // Assert
    assert(s === "((((X->4)->3)->2)->1)")
  }

  "length" should "return the length of the List" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val len = List.length(l)

    // Assert
    assert(len === 4)
  }

  def foldLeftTest[A, B](foldLeftImpl: (List[Int], String) => ((String, Int) => String) => String): Assertion = {
    // Arrange
    val l = List(1, 2, 3, 4)
    val f = (s: String, i: Int) => s"($s->$i)"

    // Act
    val s = foldLeftImpl(l, "X")(f)

    // Assert
    assert(s === "((((X->1)->2)->3)->4)")
  }


  "foldLeft" should "work properly, even with non-commutative functions" in {
    foldLeftTest(List.foldLeft)
  }

  "sumLeft, productLeft and lengthLeft" should "return their correct values" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val s = List.sumLeft(l)
    val p = List.productLeft(l)
    val len = List.lengthLeft(l)

    // Assert
    assert(s === 10)
    assert(p === 24)
    assert(len === 4)
  }

  "reverse" should "reverse a List" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val r = List.reverse(l)

    // Assert
    assert(r === List(4, 3, 2, 1))
  }

  "foldLeftR" should "behave like foldLeft" in {
    foldLeftTest(List.foldLeftR)
  }

  "append" should "append an element at the end" in {
    // Arrange
    val l = List(1, 2, 3, 4)

    // Act
    val a = List.append(l, 5)

    // Assert
    assert(a === List(1, 2, 3, 4, 5))
  }

  "flatten" should "concatenate Lists of the same type" in {
    // Arrange
    val l = List(
      List(1, 2, 3),
      List(4, 5),
      Nil,
      List(6)
    )

    // Act
    val f = List.flatten(l)

    // Assert
    assert(f === List(1, 2, 3, 4, 5, 6))
  }

  "addOne" should "increase the values of elements of the List by one" in {
    // Arrange
    val l = List(1, 54, 2)

    // Act
    val a = List.addOne(l)

    // Assert
    assert(a === (List(2, 55, 3)))
  }

  "doubleToString" should "convert doubles to strings" in {
    // Arrange
    val l = List(7, 2.4, 8)

    // Act
    val d = List.doubleToString(l)

    // Assert
    assert(d === List("7.0", "2.4", "8.0"))
  }

  "map" should "behave like normal map" in {
    // Arrange
    val l = List(1, 3, 5, 7)
    val f = (x: Int) => s"[$x]"

    // Act
    val m = List.map(l)(f)

    // Assert
    assert(m === List("[1]", "[3]", "[5]", "[7]"))
  }

  def filterTest(filterImpl: List[Int] => (Int => Boolean) => List[Int]): Assertion = {
    // Arrange
    val l = List(1, 2, 3, 5, 6, 7)
    val f = (x: Int) => x % 2 == 0

    // Act
    val filt = filterImpl(l)(f)

    // Assert
    assert(filt === List(2, 6))
  }

  "filter" should "filter items from the List" in {
    filterTest(List.filter)
  }

  "flatMap" should "behave like normal flatMap" in {
    // Arrange
    val l = List(1, 2, 3)
    val f = (x: Any) => List(x, x) // Note that this method erases the type

    // Act
    val fm = List.flatMap(l)(f)

    // Assert
    assert(fm === List(1, 1, 2, 2, 3, 3))
  }

  "filterFM" should "behave like filter" in {
    filterTest(List.filterFM)
  }

  "add" should "add numbers of two Lists" in {
    // Arrange
    val as = List(1, 2, 3, 4, 5)
    val bs = List(3, 5, 6, 7, 3)

    // Act
    val s = List.add(as, bs)

    // Assert
    assert(s === List(4, 7, 9, 11, 8))
  }

  "zipWith" should "zip two lists" in {
    // Arrange
    val as = List(1, 2, 3, 4)
    val bs = List(4, 3, 2, 5)
    val f = (x: Int, y: Int) => x + 2 * y

    // Act
    val z = List.zipWith(as, bs)(f)

    // Assert
    assert(z === List(9, 8, 7, 14))
  }

  "hasSubsequence" should "find out whether the subsequence is included" in {
    // Arrange
    val xs = List(1, 2, 3, 3, 3, 4, 5, 6)
    val subt = List(3, 3, 3)
    val subf = List(3, 4, 3)

    // Act
    val t = List.hasSubsequence(xs, subt)
    val f = List.hasSubsequence(xs, subf)

    // Assert
    assert(t === true)
    assert(f === false)
  }

  // Arrange
  val t = Branch(
    Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Leaf(3)
    ),
    Leaf(5)
  )

  def sizeTest(sizeImpl: Tree[Int] => Int): Assertion = {
    // Act
    val s = sizeImpl(t)

    // Assert
    assert(s === 7)
  }

  def maximumTest(maximumImpl: Tree[Int] => Int): Assertion = {
    // Act
    val m = maximumImpl(t)

    // Assert
    assert(m === 5)
  }

  def depthTest(depthImpl: Tree[Int] => Int): Assertion = {
    // Act
    val d = depthImpl(t)

    // Assert
    assert(d === 4)
  }

  def mapTest(mapImpl: Tree[Int] => (Int => String) => Tree[String]): Assertion = {
    // Arrange
    val f = (x: Int) => s"[$x]"

    // Act
    val m = mapImpl(t)(f)

    // Assert
    assert(m ===
      Branch(
        Branch(
          Branch(
            Leaf("[1]"),
            Leaf("[2]")
          ),
          Leaf("[3]")
        ),
        Leaf("[5]")
      ))
  }

  "size" should "return the size of the Tree" in {
    sizeTest(Tree.size)
  }

  "maximum" should "return the maximum of the Tree" in {
    maximumTest(Tree.maximum)
  }

  "depth" should "return the depth of the Tree" in {
    depthTest(Tree.depth)
  }

  "map" should "behave like a normal map" in {
    mapTest(Tree.map)
  }

  "fold" should "result in correct size, maximum, depth and map functions" in {
    sizeTest(Tree.sizeF)
    maximumTest(Tree.maximumF)
    depthTest(Tree.depthF)
    mapTest(Tree.mapF)
  }
}
