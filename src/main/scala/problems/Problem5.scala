package problems

import utils.Utils._

import scala.annotation.tailrec

/**
  * Smallest multiple
  *
  * Problem 5
  * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  *
  * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
  */
object Problem5 extends App {

  def smallestDivisibleByRange(largerDivisor: Int): Long = {

    def obtainMCMFactors(factorizations: List[List[(Int, Int)]]): List[(Int, Int)] = {

      def insertBigger(prime: Int, exponent: Int, insertingList: List[(Int, Int)]): List[(Int, Int)] = {
        @tailrec
        def loopInsertBigger(unprocessed: List[(Int, Int)], processed: List[(Int, Int)]): List[(Int, Int)] = {
          unprocessed match {
            case (`prime`, e) :: t =>
              if (e > exponent) (prime, e) :: processed ::: t
              else (prime, exponent) :: processed ::: t
            case other :: t => loopInsertBigger(t, other :: processed)
            case Nil => (prime, exponent) :: processed
          }
        }

        loopInsertBigger(insertingList, Nil)
      }

      @tailrec
      def addFactors(unprocessed: List[(Int, Int)], result: List[(Int, Int)]): List[(Int, Int)] = {
        unprocessed match {
          case (p, e) :: t =>
            val r = insertBigger(p, e, result)
            addFactors(t, r)
          case Nil => result
        }
      }

      @tailrec
      def loopObtainMCMFactors(unprocessed: List[List[(Int, Int)]], result: List[(Int, Int)]): List[(Int, Int)] = {
        unprocessed match {
          case factors :: t =>
            val r = addFactors(factors, result)
            loopObtainMCMFactors(t, r)
          case Nil => result
        }
      }

      loopObtainMCMFactors(factorizations, Nil)
    }

    val result = obtainMCMFactors((1L to largerDivisor).map(getPrimeFactorization).toList)
    result.foldLeft(1L) {
      case (r, (p, e)) => r * Math.pow(p, e).toLong
    }

  }

  measure(smallestDivisibleByRange(20))

}
