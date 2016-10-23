
object Test extends App {

  import SemigroupInstances._
  import FunctorInstances._


  assert(SemigroupEvidence[Int].combine(1, 2) == 3)
  assert(SemigroupEvidence[String].combine("a", "b") == "ab")
  // if using :
  //assert(FunctorEvidence[Option[Int]].newMap(Some(2))(x => x + 1) == Some(3))
  // fails with the following error: Option[Int] takes no type parameters, expected: one
  // it means that it expects a higher-kinded type with a type parameter, not a concrete type like Option[Int]
  assert(FunctorEvidence[Option].newMap(Some(2))(x => x + 1) == Some(3))
  assert(FunctorEvidence[Option].newMap(None)((x: Int) => x + 1) == None)
}
