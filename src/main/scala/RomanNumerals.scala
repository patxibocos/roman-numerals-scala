object RomanNumerals extends App {

  val romanValues = Map(1 -> "I", 5 -> "V", 10 -> "X", 50 -> "L", 100 -> "C", 500 -> "D", 1000 -> "M")
  val sortedKeys = romanValues.keys.toList.sorted

  def toRoman(n: Int): String = {
    if (n == 0) "" else {
      val digits = Math.floor(Math.log10(Math.abs(n))) + 1
      val zeroPart = Math.pow(10, digits - 1).toInt
      val value = n / zeroPart * zeroPart
      s"${getPart(value)}${toRoman(n - value)}"
    }
  }

  def getPart(n: Int): String = {
    // This getOrElse is a corner case for the lower bound
    val next = sortedKeys.find(_ >= n).getOrElse(sortedKeys.last)
    val nearest = sortedKeys.filter(_ <= n).last
    // This if is a corner case for upper bound
    val prev = if (next != nearest) sortedKeys.filter(_ < nearest).lastOption.getOrElse(nearest) else sortedKeys.last
    if (next - n == prev) {
      s"${romanValues(prev)}${romanValues(next)}"
    } else {
      s"${romanValues(nearest)}" + s"${romanValues(prev)}" * ((n - nearest) / prev)
    }
  }

  println(toRoman(1))
  println(toRoman(2))
  println(toRoman(3))
  println(toRoman(4))
  println(toRoman(5))
  println(toRoman(6))
  println(toRoman(7))
  println(toRoman(8))
  println(toRoman(9))
  println(toRoman(10))
  println(toRoman(1954))
  println(toRoman(1990))
  println(toRoman(2014))
  println(toRoman(3000))
  println(toRoman(20000))

}
