package nl.vindh.fpinscala

sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap{
    x => if(f(x)) Some(x) else None
  }
}

object Option {
  // Exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap {
    m => mean(xs.map(x => math.pow(x - m, 2)))
  }

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) | (_, None) => None
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
  }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(Some(hd), tl) => {
      val s = sequence(tl)
      s match {
        case Some(lst) => Some(Cons(hd, lst))
        case None => None
      }
    }
    case Cons(None, _) => None
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case Cons(hd, tl) => {
      val fhd = f(hd)
      fhd match {
        case Some(v) => {
          val t = traverse(tl)(f)
          t match {
            case Some(lst) => Some(Cons(v, lst))
            case None => None
          }
        }
        case None => None
      }
    }
  }

  def travSequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(opt => opt)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case l: Left[E] => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case _: Left[E] => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (l: Left[E], _) => l // There is no natural way to concatenate the errors if both return an error
    case (_, l: Left[E]) => l
    case (Right(a), Right(b)) => Right(f(a, b))
  }

  // Extra
  def map2for[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield(f(aa, bb))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(opt => opt)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case Cons(hd, tl) => {
      val ff = f(hd)
      ff match {
        case Left(e) => Left(e)
        case Right(b) => {
          val t = traverse(tl)(f)
          t match {
            case Left(e) => Left(e)
            case Right(lst) => Right(Cons(b, lst))
          }
        }
      }
    }
  }

  // Exercise 4.8
  // Thought about it; read the answer; OK.
}