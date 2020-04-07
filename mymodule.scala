object MyModule {
  def abs(n: Int) : Int =
    if (n < 0) -n
    else n

  def factorial(n: Int) : Int = {
    def go(n: Int, acc: Int) : Int =
      if(n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatResult(name: String, x: Int, f : Int => Int) : String = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, f(x))
  }

  def main(args: Array[String]) : Unit = {
    println(formatResult("absolute value", -4, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
