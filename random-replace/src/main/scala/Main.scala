import shapeless._

object Main extends App {
  import Perturbator._
  case class Person(name: String, age: Int, friends: Vector[Person])
  case class Cat(favoriteFood: String, paws: Int)

  (1 to 5).foreach(_ => println(100.perturbate))
  (1 to 5).foreach(_ => println("foo".perturbate))
  (1 to 5).foreach(_ => println(Vector("foo", "bar", "bla").perturbate))
  (1 to 5).foreach(_ => println(Vector(100, 200, 300).perturbate))

  val paul = Person("Paul", 25, Vector())
  val jacques = Person("Jacques", 28, Vector())
  val pierre = Person("Pierre", 23, Vector(paul, jacques))
  val miaou = Cat("cereal", 40)

//  implicit def personPerturbable: Perturbable[Person] = {
//    val gen = Generic[Person]
//    val enc = Perturbator[gen.Repr]
//    instance[Person] { person =>
//      val hlist = enc.perturbate(gen.to(person))
//      gen.from(hlist)
//    }
//  }

  (1 to 50).foreach(_ => println(Perturbator[Cat].perturbate(miaou)))
  (1 to 50).foreach(_ => println(Perturbator[Person].perturbate(paul)))
}
