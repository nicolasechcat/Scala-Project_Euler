package problems.from_1_to_20

import utils.Utils.{factorial, measure}

/**
 * Lattice paths
 *
 * Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
 *
 * How many such routes are there through a 20×20 grid?
 */
object Problem15 extends App {

  /**
   * Brute force method
   * Doesn't end under 1 minute
   *
   * @param x
   * @param y
   * @return
   */
  def numberOfPathsFromPosition(x: Int, y: Int): Int = {
    if (x < 0 || y < 0)
      0
    else if (List((1, 0), (0, 1)).contains((x, y)))
      1
    else
      numberOfPathsFromPosition(x - 1, y) + numberOfPathsFromPosition(x, y - 1)
  }

  /**
   * Combinatorial theory solution based in pascal triangle
   *
   * n! / (k! * (n - k)!)
   *
   * @param x
   * @param y
   * @return
   */
  def numberOfPathsFromPosition2(x: Int, y: Int): BigInt = {
    if (x < y)
      factorial(x + y) / (factorial(x) * factorial(y))
    else
      factorial(x + y) / (factorial(y) * factorial(x))
  }

  assert(measure(numberOfPathsFromPosition(2, 2)) == 6)
  // assert(measure(numberOfPathsFromPosition2(2, 2)) == 6)

  measure(numberOfPathsFromPosition2(20, 19))
  measure(numberOfPathsFromPosition2(19, 20))

  measure(numberOfPathsFromPosition2(20, 20))


}
