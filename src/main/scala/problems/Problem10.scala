package problems

import utils.Utils._

/**
  * Summation of primes
  *
  * Problem 10
  * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  *
  * Find the sum of all the primes below two million.
  */
object Problem10 extends App {

  def primesSum(maxPrime: Int): Long = {
    getPrimesBelowNumber(maxPrime).sum
  }

  measure(primesSum(10))
  measure(primesSum(2000000))
}
