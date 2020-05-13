package fpscala.tree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](tree: Tree[A])(f: (A) => B)(g: (B, B) => B) : B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](tree: Tree[A]) : Int = fold(tree)((_) => 1)(1 + _ + _)

  def depth[A](tree: Tree[A]) : Int =
    fold(tree)((_) => 0)((a, b) => 1 + (a max b))

  def map[A, B](tree: Tree[A])(f: (A) => B) : Tree[B] =
    fold(tree)(value => Leaf(f(value)) : Tree[B])(Branch(_,_))

  def maximum[A](tree: Tree[A]) : A =
    fold(tree)(value => value)(_ max _)

//  def size(tree: Tree[Int]) : Int = tree match {
//    case Leaf(_) => 1
//    case Branch(left, right) => 1 + size(left) + size(right)
//  }
//
//  def depth(tree: Tree[Int]) : Int = tree match {
//    case Leaf(_) => 0
//    case Branch(left, right) => 1 + depth(left) max depth(right)
//  }
//
//  def maximum(tree: Tree[Int]) : Int = tree match {
//    case Leaf(value) => value : Int
//    case Branch(left, right) => {
//      val leftValue : Int = left match {
//        case Leaf(value) => value : Int
//        case branch : Branch[_] => maximum(branch)
//      }
//
//      val rightValue : Int = right match {
//        case Leaf(value) => value
//        case branch : Branch[_] => maximum(branch)
//      }
//
//      if(leftValue > rightValue) leftValue else rightValue
//    }
//  }
}
