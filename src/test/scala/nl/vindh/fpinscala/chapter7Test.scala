package nl.vindh.fpinscala

import java.util.concurrent.{Callable, ForkJoinPool, TimeUnit}

import org.scalatest.{FlatSpec, Matchers}
import java.util.Calendar

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
}
