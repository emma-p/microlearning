case class Result(data: Array[Char], carry: Char)
object BinaryArithmetic {
  def sum(n1: Array[Char], n2: Array[Char]): Array[Char] = {
    val totalSum = n1.reverse.zipWithIndex.foldLeft(Result(Array[Char](), '0')) { case (acc, (charAtN1, i)) => {
      val charAtN2 = n2.reverse(i)

      val bits = sumBits(charAtN1, charAtN2, acc.carry)
      Result(acc.data :+ bits._1, bits._2)
    }
    }
    if(totalSum.carry == '1') {
      (totalSum.data :+ '1').reverse
    } else {
      totalSum.data.reverse
    }
  }

  def sumBits(c1: Char, c2: Char, carry: Char): (Char, Char) = {
    (c1, c2, carry) match {
      case ('1', '0', '0') => ('1', '0')
      case ('1', '0', '1') => ('0', '1')
      case ('0', '1', '0') => ('1', '0')
      case ('0', '1', '1') => ('0', '1')
      case ('0', '0', '0') => ('0', '0')
      case ('0', '0', '1') => ('1', '0')
      case ('1', '1', '0') => ('0', '1')
      case ('1', '1', '1') => ('1', '1')
    }

  }


  def toDouble(number: Array[Char]): Double = {
    number.reverse.zipWithIndex.foldLeft(0.0) { case (result, (c, i)) =>
      result + (Integer.parseInt(c.toString) * Math.pow(2, i))
    }
  }
}
