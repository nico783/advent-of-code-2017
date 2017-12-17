package adventofcode.day2

object Day2 {

  /**
    * Exercice 1
    */
  def checksum(worksheet: List[Int], width: Int): Int = {
    val sumRows: List[Row] => Int = sumRowsConfiguration(x => x.amplitude())
    sumRows(rows(worksheet, width))
  }

  /**
    * Exercice 2
    */
  def sumValuesDivided(worksheet: List[Int], width: Int): Int = {
    val sumRows: List[Row] => Int = sumRowsConfiguration(x => x.getDiv())
    sumRows(rows(worksheet, width))
  }

  /**
    * Define the sum of rows configuration.
    *
    * @param mapRowToValue give rules for map a row to a correspondant value
    * @param rows          list of row to sum
    * @return sum of rows
    */
  private def sumRowsConfiguration(mapRowToValue: Row => Int)(rows: List[Row]): Int = {
    def sum(elements: List[Row], acc: Int): Int = elements match {
      case Nil => acc
      case x :: xs => sum(xs, acc + mapRowToValue(x))
    }

    sum(rows, 0)
  }

  /**
    * Get list of rows from a list of Int
    *
    * @param elements list of elements to map in rows
    * @param width    numbers of columns
    * @return the list of rows
    */
  private def rows(elements: List[Int], width: Int): List[Row] = {
    def build(input: List[Int], accListInt: List[Int], accListRow: List[Row]): List[Row] = {
      if (input.isEmpty && accListInt.isEmpty) {
        accListRow
      } else if (accListInt.size == width) {
        build(input, Nil, new Row(accListInt) :: accListRow)
      } else {
        build(input.tail, input.head :: accListInt, accListRow)
      }
    }

    build(elements, Nil, Nil)
  }

  private class Row(val elements: List[Int]) {
    def amplitude(): Int = elements.max - elements.min

    def getDiv(): Int = {
      def getDiv(x: Int, list: List[Int]): Int =
        if (customDivision(x, list)._1) {
          customDivision(x, list)._2
        } else {
          val permutation = permute(list)
          getDiv(permutation.head, permutation.tail)
        }

      def permute(list: List[Int]): List[Int] = {
        list.tail :+ list.head
      }

      def customDivision(x: Int, list: List[Int]): (Boolean, Int) = {
        def divide(a: Int, b: Int): Int = {
          def minMax(x: Int, y: Int): (Int, Int) = if (x <= y) (x, y) else (y, x)

          val (min, max) = minMax(a, b)
          max / min
        }

        if (list.isEmpty) (false, 0)
        else if (x % list.head == 0 || list.head % x == 0) (true, divide(list.head, x))
        else customDivision(x, list.tail)
      }

      getDiv(elements.head, elements.tail)
    }

  }

}
