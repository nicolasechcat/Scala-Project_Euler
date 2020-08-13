package problems

import utils.Utils._

/**
  * 10001st prime
  *
  * Problem 7
  * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
  *
  * What is the 10 001st prime number?
  */
object Problem7 extends App {
  def getPrimeInPosition(pos: Int): Long = {
    def loop(number: Long, primeList: List[Long], index: Int): Long = {
      if (index == pos) {
        //        println(primeList)
        primeList.head
      }
      else if (primeList.forall(n => (number % n) != 0))
        loop(number + 1, number :: primeList, index + 1)
      else
        loop(number + 1, primeList, index)
    }

    loop(2, List(), 0)
  }

  measure(getPrimeInPosition(6))
  measure(getPrimeInPosition(10001))

}
