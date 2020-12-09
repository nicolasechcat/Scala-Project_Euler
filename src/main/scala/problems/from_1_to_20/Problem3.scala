package problems.from_1_to_20

import utils.Utils.{isCoPrime, measure}

import scala.annotation.tailrec

/**
 * Largest prime factor
 *
 * Problem 3
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 */
object Problem3 extends App {

  def largestPrimeFactor(x: Long): Long = {
    @tailrec
    def reduce(number: Long, divisor: Long): Long = {
      if (number % divisor == 0) reduce(number / divisor, divisor)
      else number
    }

    @tailrec
    def loop(n: Long, remaining: Long, r: List[Long]): List[Long] = {
      if (n <= remaining) {
        if (remaining % n == 0 && isCoPrime(n, r)) {
          loop(n + 1, reduce(remaining, n), n :: r)
        } else
          loop(n + 1, remaining, r)
      }
      else r
    }

    loop(2, x, Nil).head
  }

  measure(largestPrimeFactor(13195))
  measure(largestPrimeFactor(600851475143L))
}
