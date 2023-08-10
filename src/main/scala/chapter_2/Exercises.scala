package chapter_2

object Exercises extends App {
  "3".toInt //type: Int, value: 3

  (43).-(3).+(2) //type: Int, value: 42
  // equivalent to 43 - 3 + 2

  // Infix Operator Notation
  // Any Scala expression written a.b(c) can also be written a b c.
  // a b c d e is equivalent to a.b(c).d(e) not a.b(c.d(e))

  "the quick brown fox".split(" ")
  "the quick brown fox" split " "

  "foo".take(1)
  //equivalent to
  "foo" take 1

  1 + 2 + 3
  //equivalent to
  (1).+(2).+(3)

  println("the\nusual\tescape characters apply")
}
