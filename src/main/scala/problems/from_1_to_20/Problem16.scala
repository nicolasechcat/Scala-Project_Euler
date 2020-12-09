package problems.from_1_to_20

import utils.Utils.{BigIntPow, measure}

/*
 * Power digit sum
 *
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^1000?
 */
object Problem16 extends App {

  def sumDigits(n: BigInt): Int = {
    n.toString().split("").map(_.toInt).sum
  }

  assert(measure(sumDigits(BigInt(32768))) == 26)

  measure(sumDigits(BigIntPow(2, 1000)))
}
