package fpscala.purelyfunctionalstate.simplerng

import java.util.Random
;

trait RNG {
  def nextInt: (Int, RNG)
  def double(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val tuple = rng.nextInt
    val number = tuple._1 / Int.MaxValue.toDouble

    if (number > 0 && number < 1) {
      (number, tuple._2)
    } else {
      double(tuple._2)
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

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val randInt = rng.nextInt
    val randDouble = rng.double(randInt._2)

    ((randInt._1, randDouble._1), randDouble._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val randInt = rng.nextInt
    val randDouble = rng.double(randInt._2)

    ((randDouble._1, randInt._1), randDouble._2)
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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
}
