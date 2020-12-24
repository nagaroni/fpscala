package parallellism.par

case class Par[+A](value: A)
case object Par {
  def unit[A](value: => A): Par[A] = Par[A](value)
  def get[A](par: Par[A]): A = par.value
  def map2[A, C](a: Par[A], b: Par[A])(f: (A, A) => C) : Par[C] = {
    Par(f(a.value, b.value))
  }
}

def sum(ints: IndexedSeq[Int]): Par[Int] = {
  if (ints.size <= 1) {
    Par.unit(ints.headOption.getOrElse(0))
  } else {
    val (l, r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }
}
