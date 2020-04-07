object Exercise1 {
  /* exercise 2.1 */
  def fib(nth: Int) : Int = {
    def go(n1 : Int, n2 : Int, pos: Int) : Int = {
      val nextPos = pos - 1
      if (nextPos <= 0) n2
      else go(n2, n1 + n2, nextPos)
    }

    go(0, 1, nth)
  }

  def compare(x : Int, y: Int) : Boolean =
    x > y

  /* exercise 2.2 */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    def loop(n: Int) : Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }


  /* exercise 2.3 */
  def curry[A,B,C](f: (A, B) => C) : A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  /* exercise 2.4 */
  def uncurry[A,B,C](f: A => B => C) : (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /* exercise 2.5 */
  def compose[A,B,C](f: B => C, g: A => B) : A => C =
    (a: A) => f(g(a))
}
