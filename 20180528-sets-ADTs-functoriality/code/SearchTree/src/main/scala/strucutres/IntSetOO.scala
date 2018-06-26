package strucutres

import scala.math._

object IntSetOO {

  sealed trait IntSet {
    def contains_?(x: Int): Boolean
    def adjoin(x: Int): IntSet
    def union(other: IntSet): IntSet

    def map(f: Int => Int): IntSet = this match {
      case Empty =>
        this
      case NonEmpty(el, left, right) =>
        (left map f) union (right map f) adjoin f(el)
    }

  }

  case object Empty extends IntSet {
    def contains_?(x: Int): Boolean = false

    def adjoin(x: Int): IntSet = NonEmpty(x, Empty, Empty)

    def union(other: IntSet): IntSet = other

    override def toString = "."
  }

  case class NonEmpty(el: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains_?(x: Int): Boolean =
      if (x < el) left contains_? x
      else if (x > el) right contains_? x
      else true

    def adjoin(x: Int): IntSet =
      if (x < el) NonEmpty(el, left adjoin x, right)
      else if (x > el) NonEmpty(el, left, right adjoin x)
      else this

    def union(other: IntSet): IntSet =
      ((left union right) union other) adjoin el

    override def toString = "{" + left + el + right + "}"
  }


  def foldInOrder[A](set: IntSet, z: A)(f: (Int, A)=>A, concat: (A, A) => A): A = set match {
    case Empty =>
      z
    case NonEmpty(el, left, right) =>
      concat(foldInOrder(left, z)(f, concat), f(el,  foldInOrder(right, z)(f, concat)))
  }

  def main(args: Array[String]): Unit = {
    val set = NonEmpty(7, Empty, Empty)
    val set2 = Empty adjoin 7 adjoin 5 adjoin 12 adjoin 9 adjoin 15
    println(set)
    println(set2)
    val n = set2 map (x => x + 1)
    println(n)

    println("identity: " + set2 map (x => x))

    val set3 = Empty adjoin 7 adjoin 5 adjoin 12 adjoin 9 adjoin 15

    val linearTree = Empty adjoin 1 adjoin 2 adjoin 3 adjoin 4 adjoin 5 adjoin 6 adjoin 7
    println(linearTree)

    val balancedTree = Empty adjoin 4 adjoin 2 adjoin 3 adjoin 1 adjoin 6 adjoin 5 adjoin 7
    println(balancedTree)

    val seq = foldInOrder(balancedTree, Nil:List[Int])((x, xs) => x::xs, (xs, ys) => xs ++ ys)
    println(seq)

  }
}
