package fpscala.purelyfunctionalstate.state

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B) : State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B, C](other: State[S, B])(f: (A, B) => C) : State[S, C] =
    flatMap(a => other.map(b => f(a, b)))
  def flatMap[B](g: A => State[S, B]) : State[S, B] = State(s => {
    val (a, ss) = run(s)
    g(a).run(ss)
  })
}

object State {
  def unit[S, A](value: A) : State[S, A] = State(s => (value, s))
  def sequence[S, A](fs: List[State[S, A]]) : State[S, List[A]] = State(s => {
    fs.foldRight((List[A](), s))((state, acc) => state.map(value => acc._1 ::: List[A](value)).run(acc._2))
  })
  def modify[S](f: S => S) : State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

class CandyDispenser(machine: Machine) {
  def operate = (input: Input) => (machine: Machine) => (input, machine) match {
    case (_, Machine(_, 0, _)) => machine
    case (Coin, Machine(false, _, _)) => machine
    case (Turn, Machine(true, _, _)) => machine
    case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(true, candies -1, coins)
    case _ => Machine(true, 0, 0)
  }

  def simulateMachine(inputs: List[Input]) : State[Machine, (Int, Int)] = {
    val ds = State[Machine, Machine](s => (s, s))
    val transform = (m: Machine) => State[Machine, Machine](_ => (m, m))
    inputs.foldRight(ds)((input, state) => State[Machine, Machine](s => state.map(operate(input))))
  }

  def simulateMachineWithFor(inputs: List[Input]) : State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map ( State.modify[Machine] _ compose operate ))
    s <- State.get
  } yield (s.candies, s.coins)
}
