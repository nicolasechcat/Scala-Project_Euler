package problems.from_1_to_20

/**
 * Longest Collatz sequence
 *
 * The following iterative sequence is defined for the set of positive integers:
 *
 * n → n/2 (n is even)
 * n → 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following sequence:
 *
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 */
object Problem14 extends App {

  def collatzSequence(number: Int): List[BigInt] = {
    def loop(n: BigInt, result: List[BigInt]): List[BigInt] = {
      if (n == 1) (BigInt(1) :: result).reverse
      else if (n % 2 == 0)
        loop(n / 2, n :: result)
      else
        loop(3 * n + 1, n :: result)
    }

    loop(BigInt(number), List())
  }


  // assert(measure(collatzSequence(13)) == List(13, 40, 20, 10, 5, 16, 8, 4, 2, 1))
  // assert(measure(collatzSequenceLength(13)) == 10)

  // measure (List.range(1, 1000000).map(x => (x, collatzSequence(x).length)).maxBy(_._2))

}
