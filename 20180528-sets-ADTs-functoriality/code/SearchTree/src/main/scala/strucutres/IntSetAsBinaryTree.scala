package strucutres

import scala.math._

object IntSetAsBinaryTree {

  sealed trait IntSet {
    def contains_?(x: Int): Boolean
    def adjoin(x: Int): IntSet
    def union(other: IntSet): IntSet

    def map(f: Int => Int): IntSet = this match {
      case Empty =>
        this
      case NonEmpty(el, left, right) =>
        NonEmpty(f(el), left map f, right map f)
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

    override def toString = "{" + {left match {case Empty => left;case _ =>  left+"<-" }} + el + {right match {case Empty => right; case _ =>  "->"+right }} + "}"
  }

  def double(set: IntSet): IntSet = set match {
    case Empty                     => set
    case NonEmpty(el, left, right) => NonEmpty(2 * el, double(left), double(right))
  }

  def height(set: IntSet): Int = set match {
    case Empty                    => 0
    case NonEmpty(_, left, right) => 1 + max(height(left), height(right))
  }

  def size(set: IntSet): Int = set match {
    case Empty                    => 0
    case NonEmpty(_, left, right) => 1 + size(left) + size(right)
  }

  def f1(x: Int): Int = f2(x) * f2(x)

  def f2(x: Int): Int = f1(x) * f1(x)

  def sum(set: IntSet): Int = set match {
    case Empty =>
      0
    case NonEmpty(el, left, right) =>
      el + sum(left) + sum(right)
  }

  def product(set: IntSet): Int = set match {
    case Empty =>
      1
    case NonEmpty(el, left, right) =>
      el * product(left) * product(right)
  }

  def fold(set: IntSet, z: Int)(f: (Int, Int)=>Int): Int = set match {
    case Empty =>
      z
    case NonEmpty(el, left, right) =>
      f(el,  f(fold(left, z)(f), fold(right, z)(f)))
  }

  // TODO need to think how to implement it... need to include folding of the right
  def genericFoldPreOrder[A](set: IntSet, z: A)(f: (Int, A)=>A, concat: (A, A) => A): A = set match {
    case Empty =>
      z
    case NonEmpty(el, left, right) =>
      f(el,  concat(genericFoldPreOrder(left, z)(f, concat), genericFoldPreOrder(right, z)(f, concat)))
  }

  def genericFoldInOrder[A](set: IntSet, z: A)(f: (Int, A)=>A, concat: (A, A) => A): A = set match {
    case Empty =>
      z
    case NonEmpty(el, left, right) =>
      concat(genericFoldInOrder(left, z)(f, concat), f(el,  genericFoldInOrder(right, z)(f, concat)))
  }

  def genericFoldPostOrder[A](set: IntSet, z: A)(f: (Int, A)=>A, concat: (A, A) => A): A = set match {
    case Empty =>
      z
    case NonEmpty(el, left, right) =>
      concat(concat(genericFoldPostOrder(left, z)(f, concat), genericFoldPostOrder(right, z)(f, concat)), f(el,  z))
  }


  def main(args: Array[String]): Unit = {
    val set = NonEmpty(7, Empty, Empty)
    val set2 = Empty adjoin 7 adjoin 5 adjoin 12 adjoin 9 adjoin 15
    println(set)
    println(set2)
    val n = set2 map (x => x + 1)
    println(n)

    println(size(set))
    println(size(set2))

    println(height(set))
    println(height(set2))

    val set3 = Empty adjoin 7 adjoin 5 adjoin 12 adjoin 9 adjoin 15
    println(double(set3))

    val linearTree = Empty adjoin 1 adjoin 2 adjoin 3 adjoin 4 adjoin 5 adjoin 6 adjoin 7
    println(linearTree)

    val balancedTree = Empty adjoin 4 adjoin 2 adjoin 3 adjoin 1 adjoin 6 adjoin 5 adjoin 7
    println(balancedTree)

    val seq = genericFoldInOrder(balancedTree, Nil:List[Int])((x, xs) => x::xs, (xs, ys) => xs ++ ys)
    println(seq)

  }
}
