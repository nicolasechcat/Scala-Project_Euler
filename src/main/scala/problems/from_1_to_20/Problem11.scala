package problems.from_1_to_20

import utils.Utils.measure

import scala.annotation.tailrec

/**
 * Largest product in a grid
 *
 * Problem 11
 * In the 20×20 grid below, four numbers aInt a diagonal line have been marked in red.
 *
 * 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
 * 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
 * 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
 * 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
 * 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
 * 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
 * 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
 * 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
 * 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
 * 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
 * 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
 * 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
 * 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
 * 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
 * 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
 * 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
 * 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
 * 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
 * 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
 * 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
 *
 * The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
 *
 * What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
 */
object Problem11 extends App {

  val combinationLength = 4

  /**
   * Obtains a list with the product of each combination of 'combinationLength' horizontally adjacent numbers
   *
   * @param data Squared matrix to be inspected
   * @return
   */
  def getHorizontalCombinations(data: Array[Array[Int]]): List[Int] = {
    @tailrec
    def extractHorizontalCombinations(row: Array[Int], position: Int, result: List[List[Int]]): List[Int] =
      if (position > row.length - combinationLength) result.reverse map (_.product)
      else extractHorizontalCombinations(row, position + 1, row.slice(position, position + combinationLength).toList :: result)

    data.flatMap(x => extractHorizontalCombinations(x, 0, List())).toList
  }

  /**
   * Obtains a list with the product of each combination of 'combinationLength' vertically adjacent numbers
   *
   * @param data Squared matrix to be inspected
   * @return
   */
  def getVerticalCombinations(data: Array[Array[Int]]): List[Int] = {
    @tailrec
    def extractVerticalCombinations(x: Int, y: Int, result: List[List[Int]]): List[Int] =
      if (x > data.length - combinationLength) result.reverse map (_.product)
      else {
        if (y >= data(x).length) extractVerticalCombinations(x + 1, 0, result)
        else extractVerticalCombinations(x, y + 1, data.slice(x, x + combinationLength).map(x => x(y)).toList :: result)
      }

    extractVerticalCombinations(0, 0, List())
  }

  /**
   * Obtains a list with the product of each combination of 'combinationLength' diagonally to the right adjacent numbers
   *
   * @param data Squared matrix to be inspected
   * @return
   */
  def getRightDiagonalCombinations(data: Array[Array[Int]]): List[Int] = {
    @tailrec
    def extractRightDiagonalCombinations(x: Int, y: Int, result: List[List[Int]]): List[Int] = {
      @tailrec
      def rightDiagonalExtractor(position: Int, result: List[Int]): List[Int] = {
        if (position >= combinationLength) result.reverse
        else rightDiagonalExtractor(position + 1, data(x + position)(y + position) :: result)
      }

      if (x > data.length - combinationLength) result.reverse map (_.product)
      else {
        if (y > data(x).length - combinationLength) extractRightDiagonalCombinations(x + 1, 0, result)
        else extractRightDiagonalCombinations(x, y + 1, rightDiagonalExtractor(0, List()) :: result)
      }
    }

    extractRightDiagonalCombinations(0, 0, List())
  }

  /**
   * Obtains a list with the product of each combination of 'combinationLength' diagonally to the left adjacent numbers
   *
   * @param data Squared matrix to be inspected
   * @return
   */
  def getLeftDiagonalCombinations(data: Array[Array[Int]]): List[Int] = {
    @tailrec
    def extractLeftDiagonalCombinationProduct(x: Int, y: Int, result: List[List[Int]]): List[Int] = {
      @tailrec
      def LeftDiagonalExtractor(position: Int, result: List[Int]): List[Int] = {
        if (position >= combinationLength) result.reverse
        else LeftDiagonalExtractor(position + 1, data(x + position)(y - position) :: result)
      }

      if (x > data.length - combinationLength) result.reverse map (_.product)
      else {
        if (y >= data(x).length) extractLeftDiagonalCombinationProduct(x + 1, combinationLength - 1, result)
        else extractLeftDiagonalCombinationProduct(x, y + 1, LeftDiagonalExtractor(0, List()) :: result)
      }
    }

    extractLeftDiagonalCombinationProduct(0, combinationLength - 1, List())
  }

  /**
   * Returns the maximum product of 'combinationLength' adjacent numbers in the same direction
   *
   * @param data
   * @return
   */
  def greatestProductAdjacentNumbers(data: Array[Array[Int]]): Int = {
    (getHorizontalCombinations(data) concat getVerticalCombinations(data) concat getRightDiagonalCombinations(data) concat getLeftDiagonalCombinations(data)).max
  }

  val data_test: Array[Array[Int]] = List(
    "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08",
    "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00",
    "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65",
    "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91",
    "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80",
    "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50",
    "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70",
    "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21",
    "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72",
    "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95",
    "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92",
    "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57",
    "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58",
    "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40",
    "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66",
    "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69",
    "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36",
    "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16",
    "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54",
    "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48").
    map(_.split(" ").map(_.toInt).toArray).toArray
  //  data_test.foreach(x => println(x.mkString("\t")))

  //  println("Number of Horizontal combinations = " + getHorizontalCombinations (data_test).length)
  //  getHorizontalCombinations (data_test).foreach(x => println (x))
  //  println("Number of Vertical combinations = " + getVerticalCombinations (data_test).length)
  //  getVerticalCombinations (data_test).foreach(x => println (x))
  //  println("Number of right diagonal combinations = " + getRightDiagonalCombinations (data_test).length)
  //  getRightDiagonalCombinations (data_test).foreach(x => println (x))
  //  println("Number of right diagonal combinations = " + getLeftDiagonalCombinations (data_test).length)
  //    getLeftDiagonalCombinations (data_test).foreach(x => println (x))

  //  measure (greatestProductAdjacentNumbers(data_test))
  assert(measure(greatestProductAdjacentNumbers(data_test)) == 70600674)
}
