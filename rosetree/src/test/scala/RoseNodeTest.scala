import java.util

import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}


class RoseNodeTest extends FlatSpec with Matchers with Checkers {
  def arrayList[T](xs: T*) = {
    val l = new util.ArrayList[T]()
    xs.foreach(x => l.add(x))
    l
  }


  it should "convert a list of paths into a rosetree with counters" in {
    val paths = Array(
      arrayList(123,1024,2048,456),
      arrayList(123,456,2048),
      arrayList(1000,456),
      arrayList(1000,457),
      arrayList(31,1000,456)
    )

    val goal = 456

    val roseNode = RoseNode.convert(paths, goal)
    roseNode.asValue should be(
      ImmutableRoseNode(
        456,
        4,
        Map(
          2048 -> ImmutableRoseNode(2048, 1, Map(1024 -> ImmutableRoseNode(1024, 1, Map(123 -> ImmutableRoseNode(123, 1, Map.empty))))),
          123 -> ImmutableRoseNode(123, 1, Map.empty),
          1000 -> ImmutableRoseNode(1000, 2, Map(31 -> ImmutableRoseNode(31, 1, Map.empty)))
        )
      )
    )
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }


}


