package utils

import scala.annotation.tailrec
import scala.concurrent.duration._

object Utils {

  /**
    * Shows the result and the execution time
    *
    * @param block Function to be executed and measured
    * @tparam R Return type of the measured function
    * @return
    */
  def measure[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()

    val time = Duration(t1 - t0, NANOSECONDS)
    println("\nElapsed time: \t" + time.toNanos + " ns \t (" + time.toMillis + " ms) \t (" + time.toSeconds + " s)")
    println("\tResult: " + result + "\n")
    result
  }

  /**
   *
   * @param message Message to be printed at the beginning of the measurement
   * @param block Function to be executed and measured
   * @tparam R Return type of the measured function
   * @return
   */
  def measure[R](message: String, block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()

    val time = Duration(t1 - t0, NANOSECONDS)
    println("\t" + message + "\n")
    println("\nElapsed time: \t" + time.toNanos + " ns \t (" + time.toMillis + " ms) \t (" + time.toSeconds + " s)")
    println("\tResult: " + result + "\n")
    result
  }


  /**
    * Primes generator
    *
    * @param s Initializer
    * @return
    */
  def primeStream(s: LazyList[Int]): LazyList[Int] =
    LazyList.cons(s.head, primeStream(s.tail filter {
      _ % s.head != 0
    }))

  val primesGenerator: LazyList[Int] = primeStream(LazyList.from(2))

  /**
    * Returns a list with the firsts primesNumber primes
    *
    * @param primesNumber Number of prime number to be returned
    * @return
    */
  def getFirstPrimes(primesNumber: Int): List[Long] = {
    @tailrec
    def loop(number: Long, primeList: List[Long], index: Int): List[Long] = {
      if (index == primesNumber) primeList
      else if (primeList.forall(n => (number % n) != 0))
        loop(number + 1, number :: primeList, index + 1)
      else
        loop(number + 1, primeList, index)
    }

    loop(2, List(), 0).reverse
  }

  /**
    * Returns a list with the prime numbers below primesTop
    *
    * @param primesTop Number which is going to be greater than all the prime numbers returned
    * @return
    */
  def getPrimesBelowNumber(primesTop: Int): List[Long] = {
    @tailrec
    def loop(number: Long, primeList: List[Long]): List[Long] = {
      if (number >= primesTop) primeList
      else if (primeList.forall(n => (number % n) != 0)) {
        //        println(number)
        loop(number + 1, number :: primeList)
      }
      else
        loop(number + 1, primeList)
    }

    loop(2, List()).reverse
  }

  /**
    * Returns true if the number is co-prime with all the numbers of the list
    *
    * @param n Number to be checked
    * @param primeList List of number used during the check
    * @return
    */
  @tailrec
  def isCoPrime(n: Long, primeList: List[Long]): Boolean = {
    primeList match {
      case h :: t => if (n % h == 0) false else isCoPrime(n, t)
      case Nil => true
    }
  }

  /**
    * Returns a list of primes till the given number
    *
    * @param n Number which is going to be greater than all the prime numbers returned
    * @return
    */
  def getPrimeList(n: Long): List[Long] = {
    @tailrec
    def loop(n: Long, r: List[Long]): List[Long] = {
      //      println(r)
      if (n <= n) {
        if (isCoPrime(n, r))
          loop(n + 1, n :: r)
        else
          loop(n + 1, r)
      }
      else r
    }

    loop(2, Nil).reverse
  }

  /**
    * Returns the factorization in prime numbers of the given number
    *
    * @param number Number to be factorized
    * @return
    */
  def getPrimeFactorization(number: Long): List[(Int, Int)] = {
    def reduce(number: Long, p: Int): (Long, Int) = {
      @tailrec
      def loop(n: Long, exponent: Int): (Long, Int) = {
        if (n % p == 0) loop(n / p, exponent + 1)
        else (n, exponent)
      }

      loop(number, 0)
    }

    @tailrec
    def loop(primeList: LazyList[Int], number: Long, r: List[(Int, Int)]): List[(Int, Int)] = {
      number match {
        case 1 => r
        case _ =>
          val p = primeList.head
          val (n, v) = reduce(number, p)
          if (v == 0)
            loop(primeList.tail, n, r)
          else
            loop(primeList.tail, n, (p, v) :: r)
      }
    }

    loop(primesGenerator, number, Nil)
  }

  /**
    * Returns true if the given String is a palindrome
    *
    * @param candidate String to be checked
    * @return
    */
  def isPalindrome(candidate: String): Boolean = {
    @tailrec
    def checkPalindrome(one: List[Char], two: List[Char]): Boolean = {
      (one, two) match {
        case (h1 :: t1, h2 :: t2) => if (h1 == h2) checkPalindrome(t1, t2) else false
        case (Nil, Nil) => true
        case _ => false
      }
    }

    checkPalindrome(candidate.toList, candidate.reverse.toList)
  }

  /**
    * Returns true if the given number is a palindrome
    *
    * @param candidate Number to be checked
    * @return
    */
  def isPalindrome(candidate: Long): Boolean = {
    isPalindrome(candidate.toString)
  }

  /**
    * Rounds a Double number to the specified number of decimals
    *
    * @param value Number to be rounded
    * @param places Number of decimal places
    * @return
    */
  def roundAvoid(value: Double, places: Int): Double = {
    val scale = Math.pow(10, places)
    (value * scale) / scale
  }


}
