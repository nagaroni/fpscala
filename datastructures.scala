package fpscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) => tail
    case _ => Nil
  }
  def setHead[A](head: A, seq: List[A]): List[A] = Cons(head, seq)
  def drop[A](list: List[A], n: Int): List[A] = n match {
    case 0 => list
    case _ => drop(tail(list), n - 1)
  }

  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(as, z)((a, b) => f(b, a))
  }

  def length[A](as: List[A]) : Int = foldRight(as, 0)((_, acc) => acc + 1)
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
  def reverse[A](as: List[A]) : List[A] = foldLeft(as, List[A]())((b, a) => Cons(a, b))
}
