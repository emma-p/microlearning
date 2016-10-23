// option is a kind / higher kinded type
trait Functor[F[_]] {
  def newMap[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorInstances {
  implicit def optionFunctor = new Functor[Option] {
    override def newMap[A,B](fa: Option[A])(f: (A) => B): Option[B] = {
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
    }
  }
  // retrieves the instance of functor for the given type e.g. optionFunctor
  //equivalent to adding (implicit ev: Functor[T]) as an implicit parameter to the function that needs the param
  def FunctorEvidence[T[_]: Functor] = implicitly[Functor[T]]
}
