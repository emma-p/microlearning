case class Result(data: Vector[Char], carry: Char)

object BinaryArithmetic {
  def sum(v1: Vector[Char], v2: Vector[Char]): Option[Vector[Char]] = {
    val (n1, n2) = equalizeNumbers(v1, v2)
    if (isBinary(n1 ++ n2)) {

      val totalSum = n1.zip(n2).foldRight(Result(Vector[Char](), '0')) { case ((charAtN1, charAtN2), acc) => {
        val bits = sumBits(charAtN1, charAtN2, acc.carry)
        Result(bits._1 +: acc.data, bits._2)
      } }

      if(totalSum.carry == '1') {
        Some('1' +: totalSum.data)
      } else Some(totalSum.data)

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


  def toDouble(number: Vector[Char]): Double = {
    number.reverse.zipWithIndex.foldLeft(0.0) { case (result, (c, i)) =>
      result + (Integer.parseInt(c.toString) * Math.pow(2, i))
    }
  }

  private def equalizeNumbers(n1: Vector[Char], n2: Vector[Char]): (Vector[Char], Vector[Char]) = {
    val diff = n1.lengthCompare(n2.length)

    // additionalZeros will be empty if the two lengths are equal
    val additionalZeros = Vector.fill(Math.abs(diff)) { '0' }

    if (diff >= 0) {
      (n1, additionalZeros ++ n2)
    } else (additionalZeros ++ n1, n2)
  }

  private def charToInt(c: Char): Int = {
    Integer.parseInt(c.toString)
  }

  private def isBinary(n: Vector[Char]): Boolean = {
    n.filterNot(c => c == '1' || c == '0').isEmpty
  }
}
