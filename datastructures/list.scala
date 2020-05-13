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
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]) : Int = foldRight(as, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](as: List[A]) : List[A] = foldLeft(as, List[A]())((b, a) => Cons(a, b))
  def append[A](as: List[A], item: A) : List[A] =
    foldRight(as, List(item))((a, b) => Cons(a, b))

  def map[A, B](as: List[A])(f: (A) => B) : List[B] =
    foldRight(as, List[B]())((a, b) => Cons(f(a), b))

  def flatten[A](as: List[List[A]]) : List[A] = 
    foldRight(as, List[A]())((a, b) => foldRight(a, b)((c, d) => Cons(c, d)))

  def plusOne(as: List[Int]) : List[Int] = map(as)(_ + 1)

  def doubleToString(as: List[Double]) : List[String] = map(as)(_.toString())

  def filter[A](as: List[A])(f: (A) => Boolean) : List[A] = {
    foldRight(as, List[A]())((a, b) => f(a) match {
        case true => Cons(a, b)
        case false => b
      })
  }

  def flatMap[A, B](as: List[A])(f: (A) => List[B]) : List[B] =
    foldRight(as, List[B]())((a, b) => f(a) match {
      case Cons(h, Cons(t, Nil)) => Cons(h, Cons(t, b))
      case _ => b
    })

  def filterWithFlatMap[A](as: List[A])(f: (A) => Boolean) : List[A] = {
    flatMap(as)((a) => f(a) match {
      case true => List(a, a)
      case false => Nil
    })
  }

  def zipSum[A, B](one: List[A], two: List[A]) : List[B] = zipWith(one, two)(_ + _)

  def zipWith[A, B](list1: List[A], list2: List[A])(f: (A, A) => B) : List[B] = List(list1, list2) match {
    case Cons(Cons(h, t), Cons(Cons(g, j), Nil)) => Cons(f(h, g), zipWith(t, j)(f))
    case _ => List[B]()
  }

  def hasSubsquence[A](sub: List[A], sup: List[A]) : Boolean = {
  }
}
