package fpscala.errorhandling.either

sealed trait Either[+E, +A] {
  def map[B](f: A => B) : Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case _ => asInstanceOf[Either[E,B]]
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case _ => asInstanceOf[Either[EE, B]]
  }

  def orElse[EE >: E, B](b: => Either[EE, B]) : Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => asInstanceOf[Either[EE, B]]
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = 
    flatMap(aa => b.map(bb => f(aa, bb)))
}

case class Right[+A](value: A) extends Either[Nothing, A]
case class Left[+E](value: E) extends Either[E, Nothing]

def sequence[E, A](es: List[Either[E, A]]) : Either[E, List[A]] = es match {
  case head :: tail => sequence(tail).flatMap(tt => head.map(hh => hh :: tt))
  case _ => Right(List[A]()).asInstanceOf[Either[E, List[A]]]
}

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]) : Either[E, List[B]] = as match {
  case head :: tail => f(head).flatMap(hh => traverse(tail).map(tt => hh :: tt))
  case _ => Right(List[B]())
}
