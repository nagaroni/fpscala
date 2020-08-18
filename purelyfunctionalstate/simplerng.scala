package fpscala.purelyfunctionalstate.simplerng;

trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt : (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG) : (Int, RNG) = {
    val tuple = rng.nextInt
    if (tuple._1 > 0) {
      return tuple
    } else {
      nonNegativeInt(tuple._2)
    }
  }
}
