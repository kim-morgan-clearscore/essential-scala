package chapter_4

import java.util.Date

object Traits extends App {

  trait Visitor {
    def id: String
    def createdAt: Date
    def age: Long = new Date().getTime - createdAt.getTime
  }
  case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor
  case class User(id: String, email: String, createdAt: Date = new Date())
      extends Visitor

  trait Feline {
    def sound: String
  }

  case object Tiger extends Feline {
    val sound = "roar"
  }
  case object Panther extends Feline {
    val sound = "roar"
  }

  case class Lion(mainSize: Int) extends Feline {
    val sound = "roar"
  }

  case class Cat(food: String) extends Feline {
    val sound = "meow"
  }

  sealed trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

  sealed trait Rectangular extends Shape {
    def width: Double
    def height: Double
    val sides = 4
    override val perimeter = 2 * width + 2 * height
    override val area = width * height
  }

  case class Circle(radius: Double) extends Shape {
    val sides = 0
    val perimeter = 2 * math.Pi * radius
    val area = math.Pi * radius * radius
  }

  case class Rectangle(x: Double, y: Double) extends Rectangular {
    val width = x
    val height = y
  }

  case class Square(x: Double) extends Rectangular {
    val width = x
    val height = x
  }

  object Draw {
    def apply(shape: Shape): String = {
      shape match {
        case rectangular: Rectangular =>
          rectangular match {
            case Rectangle(x, y) => s"A rectangle of width ${x} and height ${y}"
            case Square(x)       => s"A square of width ${x}"
          }
        case Circle(radius) => s"A circle with radius ${radius}"
      }
    }
  }

  sealed trait Color {
    def shade: String

    def apply: String =
      this match {
        case Custom(_, _, _) => this.shade
        case Pink            => "pink"
        case Red             => "red"
        case Yellow          => "yellow"
      }
  }
  final case object Red extends Color {
    val shade = "dark"
  }
  final case object Yellow extends Color {
    val shade = "light"
  }
  final case object Pink extends Color {
    val shade = "light"
  }
  final case class Custom(r: Int, g: Int, b: Int) extends Color {
    val shade = if (r < 100 && g < 100 && b < 100) "light" else "dark"
  }

  object Divide {
    def apply(int1: Int, int2: Int): DivisionResult = {
      if (int2 == 0) Infinite
      else Finite(int1/int2)
    }
  }

  sealed trait DivisionResult
  final case class Finite(result: Int) extends DivisionResult
  final case object Infinite extends DivisionResult

}
