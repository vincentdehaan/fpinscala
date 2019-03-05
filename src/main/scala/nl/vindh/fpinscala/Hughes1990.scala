// This object demonstrates some of the ideas described in
//   J. Hughes, Why Functional Programming Matters

package nl.vindh.fpinscala

import scala.collection.immutable.{Stream => ScalaStream}

object Hughes1990 extends App {
  // Sect 4.1, implementing the Newton-Raphson Square Roots algorithm
  def sqrt(n: Int): Float = within(0.05f, repeat(next(n), 1.0f))

  private def next(n: Int)(prevApproximation: Float): Float
    = (prevApproximation + n / prevApproximation) / 2

  private def repeat(f: Float => Float, a: Float): ScalaStream[Float]
    = a #:: repeat(f, f(a))

  private def within(eps: Float, str: ScalaStream[Float]): Float =
    str match {
      case a #:: b #:: _ if scala.math.abs(a - b) <= eps => b
      case a #:: b #:: tl => within(eps, b #:: tl)
      case _ => throw new Exception("More elements expected!")
    }

  (1 to 25) foreach {
    i => println(s"sqrt($i) = ${sqrt(i)}")
  }
}
