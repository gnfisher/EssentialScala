package essential

object Scala {
  def main(args: Array[String]): Unit = {
    val list = Pair(1, Pair(1, Pair(1, End)))
    assert(list.sum == 3)

    val tree = Node(Leaf(1), Node(Leaf(2), Leaf(3)))
    assert(tree.sum == 6)
  }

  sealed trait IntList {
    def sum: Int =
      this match {
        case End => 0
        case Pair(num, tail) =>
          num + tail.sum
      }
  }
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  sealed trait Tree {
    def sum: Int =
      this match {
        case Leaf(element) => element
        case Node(l, r) =>
          l.sum + r.sum
      }
  }
  final case class Node(left: Tree, right: Tree) extends Tree
  final case class Leaf(element: Int) extends Tree
}
