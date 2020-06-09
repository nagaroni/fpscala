package datastructures.strictness.list;

sealed trait Stream[+A] {
  def headOption : Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  def take(n: Int) : Stream[A] = this match {
    case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int) : Stream[A] = this match {
    case Cons(h, t) => if (n == 1) t() else t().drop(n - 1)
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if(p(h())) Cons(h, () => t().takeWhile(p)) else Empty
    case _ => Empty
  }

  def exists(p: A => Boolean) : Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B) : B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean) : Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean) : Stream[A] = 
    foldRight(Empty.asInstanceOf[Stream[A]])((a, b) => if(p(a)) Cons(() => a, () => b) else Empty)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A] : Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}