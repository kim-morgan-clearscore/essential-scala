package chapter_4

object modellingData extends App {
  sealed trait TrafficLight {
    def next: TrafficLight

    def patternNext: TrafficLight = {
      this match {
        case Red    => Yellow
        case Green  => Red
        case Yellow => Green
      }
    }
  }
  case object Red extends TrafficLight {
    val next = Yellow
  }
  case object Green extends TrafficLight {
    val next = Red
  }
  case object Yellow extends TrafficLight {
    val next = Green
  }

  sealed trait Calculation
  case class Succeed(result: Int) extends Calculation
  case class Fail(message: String) extends Calculation

  object Calculator {
    def +(calculation: Calculation, int: Int): Calculation = {
      calculation match {
        case Succeed(result) => Succeed(result + int)
        case Fail(message)   => Fail(message)
      }
    }

    def -(calculation: Calculation, int: Int): Calculation = {
      calculation match {
        case Succeed(result) => Succeed(result - int)
        case Fail(message)   => Fail(message)
      }
    }

    def /(calculation: Calculation, int: Int): Calculation = {
      calculation match {
        case Succeed(result) =>
          if (int == 0) Fail("Division by zero") else Succeed(result / int)
        case Fail(message) => Fail(message)
      }
    }
  }

  assert(Calculator.+(Succeed(1), 1) == Succeed(2))
  assert(Calculator.-(Succeed(1), 1) == Succeed(0))
  assert(Calculator.+(Fail("Badness"), 1) == Fail("Badness"))

  case class BottledWater(size: Int, source: Source, carbonated: Boolean)

  sealed trait Source
  case object Well extends Source
  case object Spring extends Source
  case object Tap extends Source

  sealed trait Feline {
    def dinner: Food

    def patternDinner: Food = {
      this match {
        case Lion()             => Antelope
        case Tiger()            => TigerFood
        case Panther()          => Licorice
        case Cat(favouriteFood) => CatFood(favouriteFood)
      }
    }
  }

  final case class Lion() extends Feline {
    val dinner: Food = Antelope
  }

  final case class Tiger() extends Feline {
    val dinner: Food = TigerFood
  }

  final case class Panther() extends Feline {
    val dinner: Food = Licorice
  }

  final case class Cat(favouriteFood: String) extends Feline {
    def dinner: Food = CatFood(favouriteFood)
  }

  sealed trait Food
  case object Antelope extends Food
  case object TigerFood extends Food
  case object Licorice extends Food
  final case class CatFood(food: String) extends Food

  sealed trait IntList {
    def sum: Int =
      this match {
        case End              => 0
        case Pair(head, tail) => head + tail.sum
      }

    def foldSum: Int =
      this.fold(1, (x, y) => x + y)

    def length: Int = {
      this match {
        case End              => 0
        case Pair(head, tail) => 1 + tail.length
      }
    }

    def foldLength: Int = this.fold(0, (_, y) => 1 + y)

    def product: Int = {
      this match {
        case End              => 1
        case Pair(head, tail) => head * tail.product
      }
    }

    def foldProduct: Int = this.fold(1, (x, y) => x * y)

    def double: IntList = {
      this match {
        case End              => End
        case Pair(head, tail) => Pair(head * 2, tail.double)
      }
    }

    def foldDouble: IntList =
      this.generalFold[IntList](End, (head, tail) => Pair(head * 2, tail))

    def fold(end: Int, f: (Int, Int) => Int): Int = {
      this match {
        case End              => end
        case Pair(head, tail) => f(head, tail.fold(end, f))
      }
    }

    def generalFold[A](end: A, f: (Int, A) => A): A = {
      this match {
        case End              => end
        case Pair(head, tail) => f(head, tail.generalFold(end, f))
      }
    }
  }
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))

  println(example.foldSum)
  println(example.foldLength)
  println(example.foldProduct)
  println(example.foldDouble)

  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)

  sealed trait Tree {
    def sumPoly: Int

    def sumPattern: Int = {
      this match {
        case Node(left, right) => left.sumPattern + right.sumPattern
        case Leaf(value)       => value
      }
    }

    def doublePoly: Tree

    def doublePattern: Tree = {
      this match {
        case Node(left, right) => Node(left.doublePattern, right.doublePattern)
        case Leaf(value)       => Leaf(value * 2)
      }
    }
  }
  final case class Node(left: Tree, right: Tree) extends Tree {
    def sumPoly = (left.sumPoly + right.sumPoly)
    def doublePoly: Tree = Node(left.doublePoly, right.doublePoly)
  }
  final case class Leaf(value: Int) extends Tree {
    def sumPoly: Int = value
    def doublePoly: Tree = Leaf(value * 2)
  }

}
