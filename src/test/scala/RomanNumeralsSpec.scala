import RomanNumerals._
import org.scalatest._

class RomanNumeralsSpec extends FlatSpec with Matchers {

  "A simple number" should "be correctly translated" in {
    toRoman(1) should be("I")
    toRoman(5) should be("V")
  }

  "A number with a few rest" should "be nearest one plus the rest" in {
    toRoman(6) should be("VI")
    toRoman(7) should be("VII")
    toRoman(8) should be("VIII")
    toRoman(16) should be("XVI")
  }

  "A number near to the next number" should "be the previous nearest plus the next value" in {
    toRoman(9) should be ("IX")
    toRoman(999) should be ("CMXCIX")
  }

}