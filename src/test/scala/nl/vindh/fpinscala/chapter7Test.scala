package nl.vindh.fpinscala

import java.util.concurrent.{Callable, Executors, ForkJoinPool, TimeUnit}

import org.scalatest.{FlatSpec, Matchers}
import java.util.Calendar

import scala.collection.immutable.{List => ScalaList}
import chapter7._

import scala.util.Try

class chapter7Test extends FlatSpec with Matchers {
  def slowPar[A](a: A, ms: Int): Par[A] = es => es.submit(new Callable[A] {
    def call: A = {
      println(s"${Thread.currentThread().getName}: ${Calendar.getInstance.getTime}: Sleeping")
      Thread.sleep(ms)
      println(s"${Thread.currentThread().getName}: ${Calendar.getInstance.getTime}: Waking up")
      a
    }
  })

  def slowFilter(ms: Int): Int => Boolean = i => {
    println(s"${Thread.currentThread().getName}: ${Calendar.getInstance.getTime}: Filter sleeping")
    Thread.sleep(ms)
    println(s"${Thread.currentThread().getName}: ${Calendar.getInstance.getTime}: Filter waking up")
    i % 2 == 0
  }

  "get" should "respect the timeout" in {
    // Arrange
    val slow = slowPar(1, 1000)
    val r = Par.run(new ForkJoinPool)(slow)

    // Act
    val withTimeout = Try(r.get(900, TimeUnit.MILLISECONDS))
    val withoutTimeout = Try(r.get(1100, TimeUnit.MILLISECONDS))

    // Assert
    assert(withTimeout.isFailure)
    assert(withoutTimeout === Try(1))
  }

  "map2" should "respect the timeout" in {
    // Arrange
    val slowA = slowPar(1, 500)
    val slowB = slowPar(2, 1000)
    val m = Par.map2(slowA, slowB)(_ + _)
    val r = Par.run(new ForkJoinPool)(m)

    // Act
    val withTimeout = Try(r.get(700, TimeUnit.MILLISECONDS))
    val withoutTimeout = Try(r.get(1100, TimeUnit.MILLISECONDS))

    // Assert
    assert(withTimeout.isFailure)
    assert(withoutTimeout === Try(3))
  }

  "sequence" should "work as expected" in {
    // Arrange
    val ps = ScalaList(Par.unit(1), Par.unit(2), Par.unit(3))

    // Act
    val seq = Par.sequence(ps)

    // Assert
    val es = new ForkJoinPool
    // assert(seq === Par.unit(ScalaList(1, 2, 3)))
    // This does not work because function expressions do not have suitable equality functions
    assert(Par.run(es)(seq).get === ScalaList(1, 2, 3))
  }

  "Par.parFilter" should "run the filter in parallel" in {
    // Arrange
    val as = ScalaList(1, 2, 3)

    // Act
    val f = Par.parFilter(as)(slowFilter(1000))

    // Assert
    val es = new ForkJoinPool
    assert(Try(Par.run(es)(f).get(1100, TimeUnit.MILLISECONDS)) === Try(ScalaList(2)))
  }

  // Exercise 7.9
  "fork" should "deadlock in fixed size thread pool" in {
    // Arrange
    def recursiveFork(n: Int): Par[Int] =
      if(n > 0) Par.fork(recursiveFork(n - 1))
      else Par.unit(42)
    val es7 = Executors.newFixedThreadPool(6)
    val par5 = recursiveFork(5)
    val par6 = recursiveFork(6)
    val par7 = recursiveFork(7)

    // Act
    val get5 = Try(Par.run(es7)(par5).get(1000, TimeUnit.MILLISECONDS))
    val get6 = Try(Par.run(es7)(par6).get(1000, TimeUnit.MILLISECONDS))
    val get7 = Try(Par.run(es7)(par7).get(1000, TimeUnit.MILLISECONDS))

    // Assert
    assert(get5 === Try(42))
    assert(get6 === Try(42))
    assert(get7.isFailure)
  }
}
