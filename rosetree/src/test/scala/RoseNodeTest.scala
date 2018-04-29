import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers

import scala.collection.mutable.ArrayBuffer

class RoseNodeTest extends FlatSpec with Matchers with Checkers {
  it should "convert a list of paths into a rosetree with counters" in {
    val paths = List(List(123,1024,2048,456),
      List(123,456,2048),
      List(1000,456),
      List(1000,457),
      List(31,1000,456)
    )

    val goal = 456

    val roseNode = RoseNode.convert(paths, goal)
    roseNode.asValue should be(
      ImmutableRoseNode(
        456,
        4,
        ArrayBuffer(
          ImmutableRoseNode(2048, 1, ArrayBuffer(ImmutableRoseNode(1024, 1, ArrayBuffer(ImmutableRoseNode(123, 1, ArrayBuffer()))))),
          ImmutableRoseNode(123, 1, ArrayBuffer()),
          ImmutableRoseNode(1000, 2, ArrayBuffer(ImmutableRoseNode(31, 1, ArrayBuffer())))
        )
      )
    )

  }

//  "benchmark" should "benchmark" in {
//    val myGen: Gen[(List[List[Int]], Int)] =
//      for {
//        paths <- Gen.listOfN[List[Int]](1000,Gen.listOfN(1000, Gen.choose(1, 5000)))
//        goal <- Gen.choose(1,5000)
//      } yield (paths, goal)
//
//    check {
//      forAll(myGen) { case (paths, goal) =>
//        time {
//          RoseNode.convert(paths, goal)
//        }
//        1 == 1
//      }
//
//    }
//  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }


}


