case class Result(data: Array[Char], carry: Char)

object BinaryArithmetic {
  def sum(n1: Array[Char], n2: Array[Char]): Option[Array[Char]] = {
    if (isBinary(n1 ++ n2)) {
      val totalSum = n1.zip(n2).foldRight(Result(Array[Char](), '0')) { case ((charAtN1, charAtN2), acc) => {
        val bits = sumBits(charAtN1, charAtN2, acc.carry)
        Result(bits._1 +: acc.data, bits._2)
      } }
      if(totalSum.carry == '1') {
        Some('1' +: totalSum.data)
      } else {
        Some(totalSum.data)
      }
    } else None
  }

  def sumBits(c1: Char, c2: Char, carry: Char): (Char, Char) = {
    charToInt(c1) + charToInt(c2) + charToInt(carry) match {
      case 0 => ('0', '0')
      case 1 => ('1', '0')
      case 2 => ('0', '1')
      case 3 => ('1', '1')
      case _ => throw new IllegalStateException
    }
  }


  def toDouble(number: Array[Char]): Double = {
    number.reverse.zipWithIndex.foldLeft(0.0) { case (result, (c, i)) =>
      result + (Integer.parseInt(c.toString) * Math.pow(2, i))
    }
  }

  private def charToInt(c: Char): Int = {
    Integer.parseInt(c.toString)
  }
  private def isBinary(n: Array[Char]): Boolean = {
    n.filterNot(c => c == '1' || c == '0').isEmpty
  }
}
