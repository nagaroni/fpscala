package parallellism.par

case class Par[+A](value: A)
case object Par {
  def unit[A](value: => A): Par[A] = Par[A](value)
  def get[A](par: Par[A]): A = par.value
}

def sum(ints: IndexedSeq[Int]): Int = {
  if (ints.size <= 1) {
    ints.headOption.getOrElse(0)
  } else {
    val (l, r) = ints.splitAt(ints.length/2)
    val sumL: Par[Int] = Par.unit(sum(l))
    val sumR: Par[Int] = Par.unit(sum(r))
    Par.get(sumL) + Par.get(sumR)
  }
}
