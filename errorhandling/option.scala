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
    map(value => Some(value)).getOrElse(ob)

  def filter(f: A => Boolean) : Option[A] =
    flatMap((value) =>  if (f(value)) Some(value) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


// use Option to calculate variance
def variance(xs: Seq[Double]): Option[Double] =  {
  Some(xs).flatMap((sq) => 
    try {
      val size = sq.size
      val mean = sq.sum / size
      Some(sq.map((x) => math.pow(x - mean, 2)).sum / size)
    } catch { case _ : Throwable => None }
  )
}

def lift[A, B](f: A => B) : Option[A] => Option[B] = _ map f

def Try[A](a: => A) : Option[A] =
  try(Some(a))
  catch { case e : Exception => None }

def insuranceRateQuote(age: Int, numberOfSpeedsTicket: Int): Double =
  Some(numberOfSpeedsTicket).filter(_ > 0).map(divisor => age / numberOfSpeedsTicket.toDouble ).getOrElse(0.0)

def parseInsuranceRateQuote(age: String, numberOfSpeedsTicket: String): Option[Double] =  {
  val optAge : Option[Int] = Try(age.toInt)
  val optTickets : Option[Int] = Try(numberOfSpeedsTicket.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
}

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = {
  // val ff = lift((a: A) => lift((b: B) => f(a, b)))
  // ff(a).flatMap(lifted => lifted(b))
  // a.map(value => (b: B) => f(value, b)).flatMap(func => lift(func)(b))
  a.flatMap(aa => b.map(bb => f(aa, bb)))
}

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  Some(a).filter(_.size > 0).map(_.head).flatMap(value => {
      val tail = a.tail
      if(tail.isEmpty) {
        value.map(v => List(v))
      } else {
        sequence(tail).flatMap(t => value.map(v => v :: t))
      }
    }
  )
}

def traverse[A, B](a: List[A])(f: A => Option[B]) : Option[List[B]] = a match {
  case head :: tail => f(head).flatMap(value => traverse(tail)(f).map(t => value :: t))
  case Nil => Some(List[B]())
}

def sequence2[A](a: List[Option[A]]) : Option[List[A]] = {
  traverse(a)(x => x.map(v => v))
}

def map2[A, B,C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = 
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)