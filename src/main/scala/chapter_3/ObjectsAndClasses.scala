package chapter_3

object ObjectsAndClasses extends App {
  def badness = throw new Exception("Bad")
  def otherbadness = null
  val bar = if(true) 123 else badness
  val baz = if(false) "it worked" else otherbadness

  class Cat(val name: String, val colour: String, val food: String) {
  }

  val oswald = new Cat("Oswald", "Black", "Milk")
  val henderson = new Cat("Henderson", "Ginger", "Chips")
  val quentin = new Cat("Quentin", "Tabby and white", "Curry")

  assert(oswald.name == "Oswald")
  assert(henderson.colour == "Ginger")
  assert(quentin.food == "Curry")

  object ChipShop {
    def willServe(cat: Cat) = {
      cat.food == "Chips"
    }
  }

  assert(ChipShop.willServe(oswald) == false)
  assert(ChipShop.willServe(henderson) == true)

  class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
    def name = s"${firstName} ${lastName}"
  }

  class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
    def directorsAge = yearOfRelease - director.yearOfBirth
    def isDirectedBy(director: Director) = director == this.director
    def copy(name: String = this.name,
             yearOfRelease: Int = this.yearOfRelease,
             imdbRating: Double = this.imdbRating,
             director: Director = this.director) =
      new Film(name, yearOfRelease, imdbRating, director)
  }

  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "Some Body", 1990)
  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)
  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  assert(eastwood.yearOfBirth == 1930)
  assert(dieHard.director.name == "John McTiernan")
  assert(invictus.isDirectedBy(nolan) == false)

  class Adder(amount: Int) {
    def add(in: Int) = in + amount
  }

  class Counter(val count: Int) {
    def dec: Counter = dec()
    def inc: Counter = inc()
    def inc(amount: Int = 1): Counter = new Counter(count + amount)
    def dec(amount: Int = 1): Counter = new Counter(count - amount)
    def adjust(adder: Adder): Counter = new Counter(adder.add(count))
  }

  val answer = new Counter(10).inc(10).count

  println(new Counter(10).inc.dec.inc.inc.count)

  // Up to Object as Functions

}
