object RomanNumerals extends App {

  lazy val romanValues = Map(
    1 -> "I",
    5 -> "V",
    10 -> "X",
    50 -> "L",
    100 -> "C",
    500 -> "D",
    1000 -> "M"
  )
  lazy val sortedKeys = romanValues.keys.toList.sorted

  def isCloseEnough(n: Int, prev: Int, next: Int): Boolean = {
    next - n == prev
  }

  def translatePart(n: Int): String = {
    val nearest = sortedKeys.filter(_ <= n).last
    val roman = romanValues(nearest)
    val rest = n - nearest
    if (rest == 0) roman else {
      val next = sortedKeys.find(_ >= n).getOrElse(sortedKeys.last)
      val prev = if (next != nearest) sortedKeys.filter(_ < nearest).lastOption.getOrElse(nearest) else sortedKeys.last
      if (isCloseEnough(n, prev, next)) {
        s"${romanValues(prev)}${romanValues(next)}"
      } else {
        s"$roman${toRoman(rest)}"
      }
    }
  }

  def toRoman(n: Int): String = {
    if (n == 0) {
      ""
    } else {
      // If we have here 9876, we will translate 9000, then 800, then 70, then 6
      val digits = Math.floor(Math.log10(Math.abs(n))) + 1
      val zeroPart = Math.pow(10, digits - 1).toInt
      val part = n / zeroPart * zeroPart
      def rest = n - part
      s"${translatePart(part)}${toRoman(rest)}"
    }
  }


}

/*object RomanNumerals extends App {

  lazy val romanValues = Map(
    1 -> "I",
    5 -> "V",
    10 -> "X",
    50 -> "L",
    100 -> "C",
    500 -> "D",
    1000 -> "M"
  )
  lazy val sortedKeys = romanValues.keys.toList.sorted

  def isCloseEnough(n: Int, prev: Int, next: Int): Boolean = {
    next - n == prev
  }

  def toRoman(n: Int): String = {
    val nearest = sortedKeys.filter(_ <= n).last
    val roman = romanValues(nearest)
    val rest = n - nearest
    if (rest == 0) roman else {
      val next = sortedKeys.find(_ >= n).getOrElse(sortedKeys.last)
      val prev = if (next != nearest) sortedKeys.filter(_ < nearest).lastOption.getOrElse(nearest) else sortedKeys.last
      if (isCloseEnough(n, prev, next)) {
        s"${romanValues(prev)}${romanValues(next)}"
      } else {
        s"$roman${toRoman(rest)}"
      }
    }
  }

}*/

/*object RomanNumerals extends App {

  lazy val romanValues = Map(
    1 -> "I",
    5 -> "V",
    10 -> "X",
    50 -> "L",
    100 -> "C",
    500 -> "D",
    1000 -> "M"
  )
  lazy val sortedKeys = romanValues.keys.toList.sorted

  def toRoman(n: Int): String = {
    val nearest = sortedKeys.filter(_ <= n).last
    val roman = romanValues(nearest)
    val rest = n - nearest
    if (rest == 0) roman else s"$roman${toRoman(rest)}"
  }

}*/


/*object RomanNumerals extends App {

  lazy val romanValues = Map(
    1 -> "I",
    5 -> "V",
    10 -> "X",
    50 -> "L",
    100 -> "C",
    500 -> "D",
    1000 -> "M"
  )
  lazy val sortedKeys = romanValues.keys.toList.sorted

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

  println(toRoman(0))
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
*/