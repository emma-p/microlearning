
import org.scalatest.{Matchers, FlatSpec}

class BinaryArithmeticTest extends FlatSpec with Matchers {

  "sum" should "sum two positive binary literals of the same lengths" in {
    val n1 = "101010"
    val n2 = "111111"

    val parsedn1 = Integer.parseInt(n1, 2)
    val parsedn2 = Integer.parseInt(n2, 2)

    val result = Integer.parseInt(BinaryArithmetic.sum(n1.toCharArray,n2.toCharArray).mkString(""), 2)
    result should be(parsedn1 + parsedn2)
  }

  "toDouble" should "convert a binary literal to its value as Double" in {
    BinaryArithmetic.toDouble("000111".toCharArray) should be(Integer.parseInt("000111", 2))
    BinaryArithmetic.toDouble("100011".toCharArray) should be(Integer.parseInt("100011", 2))
  }
}
