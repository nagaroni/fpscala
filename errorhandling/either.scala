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
  case head :: tail => f(head).flatMap(hh => traverse(tail)(f).map(tt => hh :: tt))
  case _ => Right(List[B]())
}

def sequence2[E, A](es: List[Either[E, A]]) : Either[E, List[A]] =
  traverse(es)(x => x.map(v => v))


case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String) : Either[String, Name] =
  if(name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))

def mkAge(age: Int) : Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))

def mkPerson(name: String, age: Int) : Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))