package problems

import utils.Utils._

import scala.annotation.tailrec

/*
  * Special Pythagorean triplet
  *
  * Problem 9
  * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  *
  * a^2 + b^2 = c^2
  * For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
  *
  * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  * Find the product abc.
  */
object Problem9 extends App {

  def pythagoreanTriplet(additionValueExpected: Int): Long = {
    @tailrec
    def loop(a: Int, b: Int): Long = {
      val c = Math.sqrt(a * a + b * b)
      if (!c.isWhole) loop(a, b + 1)
      else {
        val addition = a + b + c
        if (addition == additionValueExpected) a * b * c.toLong
        else if (addition < additionValueExpected) loop(a, b + 1)
        else if (a < additionValueExpected - 2) loop(a + 1, 1)
        else -1
      }
    }

    loop(1, 1)
  }

  measure(pythagoreanTriplet(12))
  measure(pythagoreanTriplet(1000))
}
