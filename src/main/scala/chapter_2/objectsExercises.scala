package chapter_2

object objectsExercises extends App {
  object Cat1 {
    val name: String = "Oswald"
    val colour: String = "Black"
    val food: String = "Milk"
  }

  object Cat2 {
    val name: String = "Henderson"
    val colour: String = "Ginger"
    val food: String = "Chips"
  }

  object Cat3 {
    val name: String = "Quentin"
    val colour: String = "Tabby and white"
    val food: String = "Curry"
  }

  object Calc {
    def square(n: Double): Double = n * n
    def cube(n: Double): Double = square(n) * n
  }

  assert(Calc.square(2.0) == 4.0)

  object Calc2 {
    def square(n: Double): Double = n * n
    def cube(n: Double): Double = square(n) * n
    def square(n: Int): Int = n * n
    def cube(n: Int): Int = square(n) * n
  }

  object argh {
    def a = {
      println("a")
      1
    }

    val b = {
      println("b")
      a + 2
    }

    def c = {
      println("c")
      a
      b + "c"
    }
  }

  val answer = argh.c + argh.b + argh.a

  println(answer)

  object Person {
    val firstName: String = "Kim"
    val lastName: String = "Morgan"
  }

  object Alien {
    def greet(person: Person.type) = s"Hello ${person.firstName}"
  }

  Alien.greet(Person) // "Hello Kim"

}
