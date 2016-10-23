trait Semigroup[T] {
  def combine(t1: T, t2: T): T
}

object SemigroupInstances {
  implicit def intSemigroup = new Semigroup[Int] {
    override def combine(i1: Int, i2: Int): Int = i1 + i2
  }

  implicit def stringSemigroup = new Semigroup[String] {
    override def combine(s1: String, s2: String): String = s1 + s2
  }
}

object Tests {
  import SemigroupInstances._

  //find the instance of this type in the implicit scope
  def SemigroupEvidence[T] = implicitly[Semigroup[T]]

  assert(SemigroupEvidence.combine(1, 2) == 3)
  assert(SemigroupEvidence.combine("a", "b") == "ab")
}
