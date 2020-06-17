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

  def headOption2: Option[A] =
    foldRight(None.asInstanceOf[Option[A]])((a, _) => Some(a))

  def map[B](f: A => B) : Stream[B] =
    foldRight(Empty.asInstanceOf[Stream[B]])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean) : Stream[A] =
    foldRight(Empty.asInstanceOf[Stream[A]])((a, b) => if(f(a)) Cons(() => a, () => b) else b)

  def append[AA >: A](st: => Stream[AA]) : Stream[AA] =
    foldRight(st)((head, tail) => Cons(() => head, () => tail).asInstanceOf[Stream[AA]])

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight(Empty.asInstanceOf[Stream[B]])((h, t) => f(h) append t)

  def mapWithUnfold[B](f: A => B) : Stream[B] =
    Stream.unfold(this)((stream) => stream match {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def takeWithUnfold(n: Int) : Stream[A] =
    Stream.unfold((n, this))(x => x._2 match {
      case Cons(h, t) => if (x._1 == 0) None else Some((h(), (x._1 - 1, t())))
      case _ => None
    })

  def takeWhileWithUnfold(f: A => Boolean) : Stream[A] =
    Stream.unfold((f, this))(x => x match {
      case (ff, Cons(h, t)) => if(ff(h())) Some((h(), (ff, t()))) else None
      case _ => None
    })

  def zipWith[AA >: A, B](st: Stream[AA])(f: (A, AA) => B) : Stream[B] =
    Stream.unfold((this, st))((x) => x match {
      case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
      case _ => None
    })

  def zipAll[B](st: Stream[B]) : Stream[(Option[A], Option[B])] =
    zipWith(st)((a,b) => (Some(a), Some(b)))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // def constant[A](a: A) : Stream[A] = Cons(() => a, () => constant(a))
  def constant[A](a: A) : Stream[A] = unfold(a)(x => Some((a, a)))

  // def from(n: Int) : Stream[Int] = Cons(() => n, () => from(n + 1))
  def from(n: Int) : Stream[Int] = unfold(n)(x => Some(x, x + 1))

  // def ones : Stream[Int] = Stream.cons(1, ones)
  def ones : Stream[Int] = unfold(1)(x => Some((x, x)))

  def empty[A] : Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // def fibs : Stream[Int] = {
  //   def walk(current: Int, last: Int) : Stream[Int] = {
  //     Cons(() => (current + last), () => walk((current + last), current))
  //   }
  //
  //   Cons(() => 0, () => Cons(() => 1, () => walk(1,0)))
  // }

  def fibs : Stream[Int] = unfold((0, 1))(x => {
    Some((x._1, (x._2, (x._1 + x._2))))
  })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] =
    f(z).map((v) => Cons(() => v._1, () => unfold(v._2)(f))).getOrElse(Empty)
}
