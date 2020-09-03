package fpscala.purelyfunctionalstate.simplerng

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B) : S => (B, S) = flatMap(a => (s) => (f(a), s))
  def map2[B, C](other: S => (B, S))(f: (A, B) => C) : S => (C, S) = flatMap(a => State.flatMap(other)(b => ss => (f(a, b), ss)))
  def flatMap[B](g: A => S => (B, S)) : S => (B, S) = State.flatMap(run)(g)
}
object State {
  def sequence[S, A](fs: List[S => (A, S)]) : S => (List[A], S) = s => {
    fs.foldRight((List[A](), s))((nextS, acc) => {
      val (v, ss) = nextS(acc._2)
      (acc._1 ::: List(v), ss)
    })
  }
  def flatMap[S, A, B](f: S => (A, S))(g: A => S => (B, S)) : S => (B, S) = s => {
    val (v, ss) = f(s)
    g(v)(ss)
  }
}

trait RNG {
  def nextInt: (Int, RNG)
  def double(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
  def unit[A](a: A): Rand[A] = rng => (a.asInstanceOf[A], rng)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def int: Rand[Int] = _.nextInt

  def double(rng: RNG): (Double, RNG) = {
    val (number, rng2) = map(nonNegativeEven)(i => i / Int.MaxValue.toDouble)(rng)
    if ( number > 0 && number < 1) {
      unit(number)(rng2)
    } else {
      double(rng2)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val tuple = rng.nextInt
    if (tuple._1 > 0) {
      return tuple
    } else {
      nonNegativeInt(tuple._2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    both(int, double)(rng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    both(double, int)(rng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val randDouble = rng.double(rng)
    val randDoubleOne = rng.double(randDouble._2)
    val randDoubleTwo = rng.double(randDoubleOne._2)

    ((randDouble._1, randDoubleOne._1, randDoubleTwo._1), randDoubleTwo._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val t = rng.nextInt
      val r = ints(count - 1)(t._2)
      (List(t._1) ::: r._1, r._2)
    } else {
      (List[Int](), rng)
    }
  }

  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    State.sequence(List.fill(count)((f: RNG) => f.nextInt))(rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a).asInstanceOf[B], rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, ra1) = ra(rng)
    val (b, rb1) = rb(ra1)

    (f(a, b), rb1)
  }

  def sequence[A](fs: List[Rand[A]]) : Rand[List[A]] = rng => {
    fs.foldRight((List[A](), rng))((nextRng, acc) => {
      val (nextNumber, newRng) = nextRng(acc._2)
      (acc._1 ::: List(nextNumber), newRng.asInstanceOf[RNG])
    })
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]) : Rand[B] = rng => {
    val (randA, newRng) = f(rng)
    g(randA)(newRng)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B) : Rand[B] = rng => {
    flatMap(s)(a => b => (f(a), b))(rng)
  }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C) : Rand[C] = rng => {
    flatMap(ra)(a => ab => flatMap(rb)(b => ba => (f(a, b), ba))(ab))(rng)
  }

  def nonNegativeLessThan(n: Int) : Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n -1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def rollDie : Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}
