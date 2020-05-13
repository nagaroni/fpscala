package fpscala.datastructures.option;

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }
  def flatMap[B](f: A => Option[B]) : Option[B] = map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B) : B = this match {
    case None => default
    case Some(value) => value
  }
  def orElse[B >: A](ob: => Option[B]) : Option[B] =
    map((value) => Some(value)).getOrElse(ob)
  def filter(f: A => Boolean) : Option[A] =
    flatMap((value) =>  if (f(value)) Some(value) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
