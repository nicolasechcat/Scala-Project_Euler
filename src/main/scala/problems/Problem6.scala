package problems

import utils.Utils._


/*
  * Sum square difference
  *
  * Problem 6
  * The sum of the squares of the first ten natural numbers is,
  *
  * 1^2 + 2^2 + ... + 10^2 = 385
  * The square of the sum of the first ten natural numbers is,
  *
  * (1 + 2 + ... + 10)^2 = 55^2 = 3025
  * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
  *
  * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
  */
object Problem6 extends App {

  def differenceSquareSum(numberLimit: Int): Long = {
    def iterate(unprocessed: List[Int], squared: Long, unsquared: Long): (Long, Long) = {
      unprocessed match {
        case h :: t => iterate(t, squared + Math.pow(h, 2).toLong, unsquared + h)
        case Nil => (squared, unsquared)
      }
    }

    val (squared, unsquared) = iterate((1 to numberLimit).toList, 0, 0)
    Math.abs(squared - Math.pow(unsquared, 2).toLong)
  }


  measure(differenceSquareSum(100))

}
