import java.util

import scala.collection.mutable.{ArrayBuffer, Map => MutableMap}
import java.util.HashMap
import scala.collection.JavaConverters._

case class ImmutableRoseNode(id: Int, counter: Int, children: Map[Int, ImmutableRoseNode])

class RoseNode(val id: Int, var counter: Int, val children: util.HashMap[Int, RoseNode]) {
  def asValue: ImmutableRoseNode = {
    ImmutableRoseNode(
      id = id,
      counter = counter,
      children = children.asScala.map(c => c._1 -> c._2.asValue).toMap
    )
  }

  def insert(path: util.List[Int]): Unit = {
    if (path.size() > 0) {
      val page = path.remove(0)
      val existingRoseNode = children.get(page)
      if (existingRoseNode == null) {
        val node = new RoseNode(page, 1, new util.HashMap())
        children.put(page, node)
        node.insert(path) //insert in the new rosenode
      } else {
        existingRoseNode.counter += 1
        existingRoseNode.insert(path)
      }
    }
  }
}

object RoseNode {
  def convert(paths: Array[util.ArrayList[Int]], goal: Int): RoseNode = {
    val rootNode = new RoseNode(goal, 0, new util.HashMap)
    paths.foreach{ path =>
      val goalIndex = path.indexOf(goal)

      if (goalIndex > -1) {
        val relevantPath = path.subList(0, goalIndex)
        util.Collections.reverse(relevantPath)
        rootNode.counter += 1
        rootNode.insert(relevantPath)
      }
    }
    rootNode
  }
}
