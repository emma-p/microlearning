trait Semigroup[T] {
  def combine(t1: T, t2: T): T
}

object SemigroupInstances {
  implicit def intSemigroup = new Semigroup[Int] {
    override def combine(i1: Int, i2: Int): Int = i1 + i2
  }

  // new Semigroup etc. creates an anonymous class
  implicit def stringSemigroup = new Semigroup[String] {
    override def combine(s1: String, s2: String): String = s1 + s2
  }

  //find the instance of this type in the implicit scope
  // This allows to not look for the value of the evidence directly but rather to ask for the evidence for a given type
  def SemigroupEvidence[T: Semigroup] = implicitly[Semigroup[T]]
}
