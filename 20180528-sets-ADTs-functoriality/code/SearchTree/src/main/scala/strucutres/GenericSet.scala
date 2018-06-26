package strucutres


object GenericSet {

  type Set[+A] = Tree[A]

  sealed trait Tree[+A] {
  }

  case object Empty extends Tree[Nothing] {
    override def toString = "."
  }

  case class NonEmpty[A](el: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def toString: String = "{" + left + el + right + "}"
  }


  def adjoin[A](x: A, set: Set[A])(implicit cmp: Ordering[A]): Set[A] = set match {
    case Empty =>
      NonEmpty(x, Empty, Empty)
    case NonEmpty(a, left, right) =>
      if (cmp.lt(x, a)) NonEmpty(a, adjoin(x, left), right)
      else if (cmp.gt(x, a)) NonEmpty(a, left, adjoin(x, right))
      else set
  }

  def contains_?[A](x: A, set: Set[A])(implicit cmp: Ordering[A]): Boolean = set match {
    case Empty =>
      false
    case NonEmpty(a, left, right) =>
      if (cmp.lt(x, a)) contains_?(x, left)
      else if (cmp.gt(x, a)) contains_?(x, right)
      else true
  }

  def union[A](set1: Set[A], set2: Set[A])(implicit cmp: Ordering[A]): Set[A] = set1 match {
    case Empty =>
      set2
    case NonEmpty(x, left, right) => set2 match {
      case Empty =>
        set1
      case NonEmpty(_, _, _) =>
        adjoin(x, union(union(left,  right),  set2))
    }
  }

  def map[A, B](set: Set[A], f: A => B)(implicit cmp: Ordering[B]): Set[B] = set match {
    case Empty =>
      Empty
    case NonEmpty(x, left, right) =>
      val l = map(left,  f)
      val v = f(x)
      val r = map(right, f)
      adjoin(v, union(l, r))
  }


  def foldInOrder[A, B](set: Set[A], z: B)(implicit append: (A, B) => B, concat: (B, B) => B): B = set match {
    case Empty =>
      z
    case NonEmpty(x, left, right) =>
      concat(foldInOrder(left, z), append(x,  foldInOrder(right, z)))
  }

  def id(x: Int) = x
  def negate(x: Int) = -1 * x
  def double(x: Int) = 2 * x
  def square(x: Int) = x * x
  def collapse(x: Int) = 1

  def main(args: Array[String]): Unit = {
    val intSet = adjoin(7,
                 adjoin(5,
                 adjoin(12,
                 adjoin(9,
                 adjoin(15, Empty)))))

    println("== A set of integers ==")
    println(intSet)

    println("identity: " + map(intSet, id))
    println("negated: " + map(intSet, negate))
    println("doubled: " + map(intSet, double))
    println("squared: " + map(intSet, square))

    println("double compose negate: " + map(intSet, double _ compose negate))
    println("doubled after negated: " + map(map(intSet, negate), double))

    println("collapsed: " + map(intSet, collapse))

    val strSet = adjoin("Turing",
                 adjoin("McCarthy",
                 adjoin("Hoar",
                 adjoin("Church",
                 adjoin("Shannon",
                 adjoin("Cook",
                 adjoin("Knuth", Empty)))))))

    println("\n\n== A set of strings ==")
    println(strSet)
    println("identity: " + map(strSet, (s: String) => {println(s"visiting: $s");s}))
    println("collapsed: " + map(strSet, (s: String) => "word"))

    println("\n\n== From Set[String] to Set[Int] ==")
    val lengthSet: Set[Int] = map(strSet, (s: String) => s.length)
    println(lengthSet)

    val xs = foldInOrder(intSet, Vector.empty[Int])((x, xs) => x +: xs, (xs, ys) => xs ++ ys)
    println(s"\n\n$xs")
  }
}
