package strucutres

object GenericSetOO {

  type Set[A] = Tree[A]

  sealed trait Tree[A] {

    def adjoin(x: A)(implicit cmp: Ordering[A]): Set[A]

    def contains_?(x: A)(implicit cmp: Ordering[A]): Boolean

    def union(other: Set[A])(implicit cmp: Ordering[A]): Set[A]

    def map[B](f: A => B)(implicit cmp: Ordering[B]): Set[B] = this match {
      case _: Empty[A] =>
        empty
      case NonEmpty(x, left, right) =>
        val l = left map f
        val r = right map f
        val v = f(x)
        l union r  adjoin v
    }

    def foldInOrder[B](z: B)(implicit append: (A, B) => B, concat: (B, B) => B): B = this match {
      case _: Empty[A] =>
        z
      case NonEmpty(x, left, right) =>
        concat(left foldInOrder z, append(x, right foldInOrder z))
    }

  }

  case class Empty[A]() extends Tree[A] {
    def adjoin(x: A)(implicit cmp: Ordering[A]): Set[A] =
      NonEmpty(x, Empty[A], Empty[A])

    def contains_?(x: A)(implicit cmp: Ordering[A]): Boolean =
      false

    def union(other: Set[A])(implicit cmp: Ordering[A]): Set[A] =
      other

    override def toString = "."
  }

  case class NonEmpty[A](el: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
    def adjoin(x: A)(implicit cmp: Ordering[A]): Set[A] =
      if (cmp.lt(x, el)) NonEmpty(el, left adjoin x, right)
      else if (cmp.gt(x, el)) NonEmpty(el, left, right adjoin x)
      else this

    def contains_?(x: A)(implicit cmp: Ordering[A]): Boolean =
      if (cmp.lt(x, el)) left contains_? x
      else if (cmp.gt(x, el)) right contains_? x
      else true

    def union(other: Set[A])(implicit cmp: Ordering[A]): Set[A] =
      ((left union right) union other) adjoin el

    override def toString: String = "{" + left + el + right + "}"
  }

  def empty[A]: Set[A] = Empty[A]

  def id(x: Int) = x
  def negate(x: Int) = -1 * x
  def double(x: Int) = 2 * x
  def square(x: Int) = x * x
  def collapse(x: Int) = 1

  def main(args: Array[String]): Unit = {
    val set1: Set[Int] = empty adjoin 2 adjoin -1 adjoin 1
    println(set1)

    println(set1 map negate)
    println(set1 map double)
    println(set1 map square)

    println(set1 map (double _ compose negate))
    println(set1 map negate map double)

    println(set1 map collapse)

    val xs = set1.foldInOrder(Vector.empty[Int])((x, xs) => x +: xs, (xs, ys) => xs ++ ys)
    println(xs)
  }
}
