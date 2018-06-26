package strucutres

object IntSetAsUnorderedList {

  type IntSet = List[Int]

  def contains_?(x: Int, set: IntSet): Boolean =
    if (set.isEmpty)
      false
    else
      x.equals(set.head) || contains_?(x, set.tail)

  def adjoin(x: Int, set: IntSet): IntSet =
    if (contains_?(x, set))
      set
    else
      x::set

  def union(set1: IntSet, set2: IntSet): IntSet =
      if (set1.isEmpty)
        set2
      else if (set2.isEmpty)
        set1
      else
        union(set1.tail, adjoin(set1.head, set2))

  def main(args: Array[String]): Unit = {
    val set1:IntSet = adjoin(2,
                      adjoin(-2,
                      adjoin(5,
                      adjoin(2,
                      adjoin(-1,
                      adjoin(1, Nil))))))
    println(set1)

  }
}
