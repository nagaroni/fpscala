package parallellism.par;

import java.util.concurrent.{ExecutorService, TimeUnit, Callable, Future};

type Par[A] = ExecutorService => Future[A]
object Par {
  def run[A](s: ExecutorService)(a: Par[A]) : Future[A] = a(s)
  def unit[A](value: A): Par[A] = (es: ExecutorService) => UnitFuture(value)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) : A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) : Boolean = false
  }
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })
  def lazyUnit[A](value: => A) : Par[A] = fork(unit(value))
  // how to fix map2 to respect timeout from future?
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C) : Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
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

// import java.util.concurrent.ForkJoinPool
// val executor = new ForkJoinPool()
// sum(IndexedSeq(1,2,3,4,5))(executor) => 15