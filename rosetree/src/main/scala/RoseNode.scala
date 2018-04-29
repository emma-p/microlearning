import scala.collection.mutable.ArrayBuffer

case class ImmutableRoseNode(id: Int, counter: Int, children: ArrayBuffer[ImmutableRoseNode])

class RoseNode(val id: Int, var counter: Int, val children: ArrayBuffer[RoseNode]) {
  def asValue: ImmutableRoseNode = {
    ImmutableRoseNode(
      id = id,
      counter = counter,
      children = children.map(rn => rn.asValue)
    )
  }

  def insert(path: Seq[Int]): Unit = {
    path.headOption.foreach { page =>
      val pageIndexInChildren = children.indexWhere(_.id == page)
      if (pageIndexInChildren > -1) {
        val existingRoseNode = children(pageIndexInChildren)
        existingRoseNode.counter += 1
        children(pageIndexInChildren).insert(path.tail)
      } else {
        children += new RoseNode(page, 1, ArrayBuffer.empty)
        children.last.insert(path.tail) //insert in the new rosenode
      }
    }
  }

}

object RoseNode {
  def convert(paths: List[List[Int]], goal: Int): RoseNode = {
    val rootNode = new RoseNode(goal, 0, ArrayBuffer.empty)
    paths.foreach{ path =>
      val goalIndex = path.indexOf(rootNode.id)
      if (goalIndex > -1) {
        val relevantPath = path.slice(0, goalIndex).reverse
        rootNode.counter += 1
        rootNode.insert(relevantPath)
      }
    }
    rootNode
  }

}
