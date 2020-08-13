package problems

import utils.Utils._

import scala.annotation.tailrec

/**
  * Multiples of 3 and 5
  *
  * Problem 1
  * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  *
  * Find the sum of all the multiples of 3 or 5 below 1000.
  */
object Problem1 extends App {

  def sumMultiplesThreeFive(x: Long): Long = {
    @tailrec
    def loop(n: Long, r: Long): Long = {
      if (n < x) {
        if (n % 3 == 0 || n % 5 == 0) {
          //          println(s"n: $n \t\t r: " + (n + r))
          loop(n + 1, n + r)
        } else
          loop(n + 1, r)
      } else
        r
    }

    loop(2, 0)
  }

  def sumMultiplesThreeFive2(x: Long): Long =
    (1L to (x - 1)).filter(x => x % 3 == 0 || x % 5 == 0).sum


  measure(sumMultiplesThreeFive(1000))
  measure(sumMultiplesThreeFive2(1000))


}
