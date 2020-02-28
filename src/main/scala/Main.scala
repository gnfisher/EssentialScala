package essential

object Scala {
  def main(args: Array[String]): Unit = {
    val list = Pair(1, Pair(1, Pair(1, End)))
    assert(list.sum == 3)

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
}
