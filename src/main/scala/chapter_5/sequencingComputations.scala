package chapter_5

object sequencingComputations extends App {
  final case class Box[A](value: A)

  sealed trait Result[A]
  final case class Success[A](result: A) extends Result[A]
  final case class Failure[A](reason: String) extends Result[A]

  sealed trait LinkedList[A] {
    def length: Int = {
      this match {
        case End()            => 0
        case Pair(head, tail) => 1 + tail.length
      }
    }

    def contains(element: A): Boolean = {
      this match {
        case End() => false
        case Pair(head, tail) =>
          if (head == element) true else tail.contains(element)
      }
    }

    def apply(index: Int): Result[A] = {
      if (this.length <= index) Failure("Index out of bounds")
      else {
        this match {
          case End() =>
            Failure("Attempted to get element from an Empty list")
          case Pair(head, tail) =>
            if (index == 0) Success(head) else tail(index - 1)
        }
      }
    }

    def fold[B](end: B, f: (A, B) => B): B =
      this match {
        case End()            => end
        case Pair(head, tail) => f(head, tail.fold(end, f))
      }

    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case End()            => End[B]()
        case Pair(head, tail) => Pair(fn(head), tail.map(fn))
      }

  }
  final case class End[A]() extends LinkedList[A]
  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)
  assert(example.contains(3) == true)
  assert(example.contains(4) == false)
  assert(End().contains(0) == false)
  assert(example(3) == Failure("Index out of bounds"))

  sealed trait Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B
  }
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B =
      node(left.fold(node, leaf), right.fold(node, leaf))
  }
  final case class Leaf[A](value: A) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B =
      leaf(value)
  }

  val tree: Tree[String] =
    Node(
      Node(Leaf("To"), Leaf("iterate")),
      Node(
        Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))
      )
    )

  val stringTree =
    tree.fold[String](node = (a, b) => a + " " + b, leaf = str => str)
  println(stringTree)

  case class Pair2[A, B](one: A, two: B)

  val pair = Pair2[String, Int]("hi", 2)
  println(pair.one)

  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): Unit = {
      this match {
        case Left(value)  => left(value)
        case Right(value) => right(value)
      }
    }
  }
  final case class Left[A, B](value: A) extends Sum[A, B]
  final case class Right[A, B](value: B) extends Sum[A, B]

  sealed trait Maybe[A] {
    def fold[B](empty: B, full: A => B): B

    def map[B](fn: A => B): Maybe[B] = {
      this match {
        case Full(value) => Full(fn(value))
        case Empty()     => Empty[B]()
      }
    }

    def flatMap[B](fn: A => Maybe[B]): Maybe[B] = {
      this match {
        case Full(value) => fn(value)
        case Empty()     => Empty[B]()
      }
    }
  }
  final case class Full[A](value: A) extends Maybe[A] {
    def fold[B](empty: B, full: A => B): B = full(this.value)
  }
  final case class Empty[A]() extends Maybe[A] {
    def fold[B](empty: B, full: A => B): B = empty
  }

  println(example.map(x => (x * 2)).map(y => y + 1).map(z => z / 3))

}
