package problems

import utils.Utils._

import scala.annotation.tailrec

/**
  * Largest palindrome product
  *
  * Problem 4
  * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
  *
  * Find the largest palindrome made from the product of two 3-digit numbers.
  */
object Problem4 extends App {

  def largestPalindromeProduct(numDigits: Int): Long = {
    val maxNumber = (Math.pow(10, numDigits) - 1).toLong

    @tailrec
    def generateVariations(n: Long, r: List[Long]): List[Long] = {
      if (n > 1) {
        val numbers = (2L to n).toList
        generateVariations(n - 1, numbers.map(x => x * n) ::: r)
      } else
        r
    }

    generateVariations(maxNumber, Nil).filter(isPalindrome).max
  }

  measure(largestPalindromeProduct(3))

}
