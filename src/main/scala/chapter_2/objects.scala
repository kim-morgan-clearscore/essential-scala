package chapter_2

object Test {
  def name: String = "Probably the best object ever"
}

object Test2 {
  val name = "Kim" // field
  def hello(other: String): String = name + " says hello to " + other // method
}

object Objects extends App {
  println(Test2.hello("you"))
}


