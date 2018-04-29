import shapeless.{::, Generic, HList, HNil, Lazy}

import scala.util.Random

object Perturbator {
  trait Perturbable[A] {
    def perturbate(a: A): A
  }

  //Constructor method
  def instance[A](f: A => A): Perturbable[A] = {
    new Perturbable[A] {
      def perturbate(a: A): A = f(a)
    }
  }

  // Summoner method
  def apply[A](implicit pert: Perturbable[A]): Perturbable[A] =
    pert

  implicit val perturbatorInt: Perturbable[Int] = instance { x =>
    val r = new Random()
    //TODO fix - bound must be positive
    val span = x * 0.1
    x + r.nextInt(span.toInt)
  }

  implicit class PertubableOps[A: Perturbable](a: A) {
    def perturbate: A = Perturbator[A].perturbate(a)
  }

  implicit val perturbatorStr: Perturbable[String] = instance { x =>
    val r = new Random()

    val newChar = r.nextPrintableChar()
    val index = r.nextInt(x.length)
    x.toList.zipWithIndex.map{ case (c, i) =>
      if (i == index) newChar else c
    }.mkString

  }

  implicit def pertubatorVector[A](implicit ev: Lazy[Perturbable[A]]): Perturbable[Vector[A]] = instance { v =>
    val r = new Random()

    def removeElement: Vector[A] = {
      if (v.isEmpty) v
      else {
        val index = r.nextInt(v.length)
        v.zipWithIndex.filterNot { case (_, i) => i == index }.map(_._1)
      }
    }

    def duplicateAndRandomiseElement: Vector[A] = {
      if (v.isEmpty) v
      else {
        val index = r.nextInt(v.length)
        v.zipWithIndex.flatMap { case (el, i) =>
          if (i == index) {
            Vector(el, ev.value.perturbate(el))
          } else {
            Vector(el)
          }
        }
      }
    }

    if (r.nextBoolean) removeElement else duplicateAndRandomiseElement
  }

  implicit val perturbateHNil: Perturbable[HNil] =
    instance(identity)

  implicit def perturbateHList[H, T <: HList](
    implicit hPerturbable: Lazy[Perturbable[H]],
             tPerturbable: Perturbable[T])
    : Perturbable[H :: T] =
      instance { case h :: t =>
        hPerturbable.value.perturbate(h) :: tPerturbable.perturbate(t)
      }

  object Generic {
    //type refinement with { type Repr = R }.
    type Aux[A,R] = Generic[A] { type Repr = R }
  }

  implicit def genericPerturbable[A, R](implicit gen: Generic.Aux[A,R], pert: Lazy[Perturbable[R]]): Perturbable[A] =
    instance[A] { a =>
      val hlist = pert.value.perturbate(gen.to(a))
      gen.from(hlist)
    }
}
