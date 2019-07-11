package nl.vindh.fpinscala

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object chapter7 extends App {



  type Par[A] = ExecutorService => Future[A]


  object Par {
    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A]{
      def isDone: Boolean = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled: Boolean = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }


    // Exercise 7.1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      // Exercise 7.3
      // NOTE: The solution in the companion booklet handles the isDone, isCancelled and cancel methods in the right
      // way. However, it does not calculate the timeout in nanoseconds, which is supported by the Future API.
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        new Future[C] {
          def isDone: Boolean = true
          def get(timeout: Long, units: TimeUnit) = {
            val start = System.nanoTime()
            val afg = af.get(timeout, units)
            val end = System.nanoTime()
            val elapsed = units.convert(end - start, TimeUnit.NANOSECONDS)
            f(afg, bf.get((timeout - elapsed).max(0), units))
          }
          def get: C = f(af.get, bf.get)
          def isCancelled: Boolean = false
          def cancel(evenIfRunning: Boolean): Boolean = false
        }
      }
    }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  }

  // Exercise 7.2
  // case class Par[A](get: () => A)
}
