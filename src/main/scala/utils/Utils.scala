package utils

import scala.concurrent.duration._

object Utils {

  /**
    * Shows the result and the execution time
    *
    * @param block
    * @tparam R
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
    * @param s
    * @return
    */
  def primeStream(s: Stream[Int]): Stream[Int] =
    Stream.cons(s.head, primeStream(s.tail filter {
      _ % s.head != 0
    }))

  val primesGenerator = primeStream(Stream.from(2))

  /**
    * Returns a list with the firsts primesNumber primes
    *
    * @param primesNumber
    * @return
    */
  def getFirstPrimes(primesNumber: Int): List[Long] = {
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
    * @param primesTop
    * @return
    */
  def getPrimesBelowNumber(primesTop: Int): List[Long] = {
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
    * @param n
    * @param primeList
    * @return
    */
  def isCoPrime(n: Long, primeList: List[Long]): Boolean = {
    primeList match {
      case h :: t => if (n % h == 0) false else isCoPrime(n, t)
      case Nil => true
    }
  }

  /**
    * Returns a list of primes till the given number
    *
    * @param x
    * @return
    */
  def getPrimeList(x: Long): List[Long] = {
    def loop(n: Long, r: List[Long]): List[Long] = {
      //      println(r)
      if (n <= x) {
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
    * @param number
    * @return
    */
  def getPrimeFactorization(number: Long): List[(Int, Int)] = {
    def reduce(number: Long, p: Int): (Long, Int) = {
      def loop(n: Long, exponent: Int): (Long, Int) = {
        if (n % p == 0) loop(n / p, exponent + 1)
        else (n, exponent)
      }

      loop(number, 0)
    }

    def loop(primeList: Stream[Int], number: Long, r: List[(Int, Int)]): List[(Int, Int)] = {
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
    * @param candidate
    * @return
    */
  def isPalindrome(candidate: String): Boolean = {
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
    * Returns true if the given String is a palindrome
    *
    * @param candidate
    * @return
    */
  def isPalindrome(candidate: Long): Boolean = {
    isPalindrome(candidate.toString)
  }

  /**
    * Rounds a Double number to the specified number of decimals
    *
    * @param value
    * @param places
    * @return
    */
  def roundAvoid(value: Double, places: Int): Double = {
    val scale = Math.pow(10, places)
    (value * scale) / scale
  }


}
