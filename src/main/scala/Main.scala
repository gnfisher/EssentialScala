package essential

object Scala {
  def main(args: Array[String]): Unit = {
    val list = Pair(1, Pair(1, Pair(1, End)))
    assert(list.sum == 3)

    val tree = Node(Leaf(1), Node(Leaf(2), Leaf(3)))
    assert(tree.sum == 6)

    val expression = Addition(Subtraction(Number(4), Number(2)), Number(2))
    // assert(expression.eval == 4)

    assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of a negative number"))
    assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
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

  sealed trait Calculation {
    def calculate(fn: (Double, Double) => Double, right: Calculation): Calculation =
      this match {
        case Failure(m) => Failure(m)
        case Success(v) =>
          right match {
            case Failure(m) => Failure(m)
            case Success(v2) => Success(fn(v, v2))
          }
      }

  }

  final case class Success(v: Double) extends Calculation
  final case class Failure(m: String) extends Calculation

  sealed trait Expression {
    def eval: Calculation = 
      this match {
        case Number(v) => Success(v)
        case Subtraction(l,r) => l.eval.calculate((_ - _), r.eval)
        case Addition(l,r) => l.eval.calculate((_ + _), r.eval)
        case Division(l,r) => 
          l.eval match {
            case Success(v) =>
              r.eval match {
                case Success(v2) => 
                  if (v2 == 0) 
                    Failure("Division by zero")
                  else
                    Success(v / v2)
                case Failure(m) => Failure(m)
              }
            case Failure(m) => Failure(m)
          }
        case SquareRoot(e) => 
          e.eval match {
            case Failure(m) => Failure(m)
            case Success(v) => 
              if (v < 0)
                Failure("Square root of a negative number")
              else
                Success(scala.math.sqrt(v))
          }
  }
  }

  final case class Addition(l: Expression, r: Expression) extends Expression 
  final case class Subtraction(l: Expression, r: Expression) extends Expression
  final case class Number(v: Double) extends Expression
  final case class Division(l: Expression, r: Expression) extends Expression
  final case class SquareRoot(v: Expression) extends Expression}
