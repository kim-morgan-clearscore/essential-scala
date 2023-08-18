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

}
