package nl.vindh.fpinscala

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.collection.immutable.{List => ScalaList, Nil => ScalaNil}

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

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    // Exercise 7.5
    def sequence[A](ps: ScalaList[Par[A]]): Par[ScalaList[A]] =
      ps match {
        case ScalaNil => unit(ScalaNil.asInstanceOf[ScalaList[A]])
        case hd :: tl => map2(hd, sequence(tl))((phd, ptl) => phd :: ptl)
      }

    def parMap[A, B](ps: ScalaList[A])(f: A => B): Par[ScalaList[B]] = fork {
      val fbs: ScalaList[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    // Exercise 7.6
    def parFilter[A](as: ScalaList[A])(f: A => Boolean): Par[ScalaList[A]] = {
      val filt = Par.parMap(as)(a => if (f(a)) ScalaList(a) else ScalaNil.asInstanceOf[ScalaList[A]])
      Par.map(filt)(_.flatten)
    }

    // Exercise 7.11
    def choiceN[A](n: Par[Int])(choices: ScalaList[Par[A]]): Par[A] =
      es => {
        val idx = run(es)(n).get
        choices(idx)(es)
      }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(if(_) 1 else 0))(ScalaList(f, t))

    // Exercise 7.12
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => {
        val key = run(es)(key).get
        choices(key)(es)
      }

    // Exercise 7.13
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val key = run(es)(pa).get
        choices(key)(es)
      }

    def choiceWithChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(cond)(if(_) t else f)

    def choiceNWithChooser[A](n: Par[Int])(choices: ScalaList[Par[A]]): Par[A] =
      chooser(n)(choices(_))

    // Exercise 7.14
    def join[A](a: Par[Par[A]]): Par[A] =
      es => {
        val p = run(es)(a).get
        p(es)
      }

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      join(map(a)(f))

    def join_[A](a: Par[Par[A]]): Par[A] =
      es => flatMap(a)(x => x)(es)

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  }

  // Exercise 7.2
  // case class Par[A](get: () => A)
}
