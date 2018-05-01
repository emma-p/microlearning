import java.io.File
import java.util
import java.util.{Collections, Scanner}

import scala.io.Source
import scala.collection.JavaConverters._

object Main {
  def main(args: Array[String]): Unit = {
    println("Loading file into memory")
    val file = args(0)
    val lines = Source.fromFile(
      new File(file)
    ).bufferedReader().lines().iterator().asScala

    val sessions = lines.toArray.map { l =>
      val list = new util.ArrayList[Int]
      l.split(",").foreach { n =>
        list.add(n.toInt)
      }
      Collections.reverse(list)
      list
    }

    println("Pausing, ...")
    new Scanner(System.in).next()
    println("Start processing..")
    val t = System.currentTimeMillis()

    RoseNode.convert(sessions, 50)

    val end = System.currentTimeMillis()

    println(s"Processing took ${end - t} ms")
  }
}
