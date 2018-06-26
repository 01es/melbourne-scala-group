package strucutres

object GenericSetAsUnorderedList {

  type Set[+A] = List[A]

  def empty[A]: Set[A] = Nil

  def contains_?[A](x: A, set: Set[A]): Boolean =
    if (set.isEmpty)
      false
    else
      x.equals(set.head) || contains_?(x, set.tail)

  def adjoin[A](x: A, set: Set[A]): Set[A] =
    if (contains_?(x, set))
      set
    else
      x::set

  def union[A](set1: Set[A], set2: Set[A]): Set[A] = set1 match {
    case Nil =>
      set2
    case x::xs => set2 match {
      case Nil =>
        set1
      case _ =>
        union(xs, adjoin(x, set2))
    }
  }

}
