package adventofcode.day3

object Day3 {
  val SPIRAL_START = 2
  val DISTANCE_START = 1
  val DISTANCE_MIN_FIRST_LOOP = 1
  val DISTANCE_MAX_FIRST_LOOP = 2


  /**
    * Give manathan distance between a point of the spiral (model by an integer >1) and the center of the spiral (1).
    * Note: particular case of manathanDistanceToSpiralCenter(1) == 0 is not treated.
    *
    * @param n
    * @return
    */
  def manathanDistanceToSpiralCenter(n: Int): Int = {
    def distance(i: Int, rho: Int, min: Int, max: Int, increase: Boolean): Int =
      if (i == n) rho
      else if (isSquareOfOdd(i))
        distance(i + 1, rho + 1, min + 1, max + 2, false)
      else {
        val rhoNew = increase match {
          case true => if (rho == max) rho - 1 else rho + 1
          case false => if (rho == min) rho + 1 else rho - 1
        }
        val increaseNew = if (rho == max || rho == min) !increase else increase
        distance(i + 1, rhoNew, min, max, increaseNew)
      }

    def isSquareOfOdd(n: Int) = {
      val root: Double = Math.sqrt(n)
      root % 1 == 0 && root % 2 == 1
    }

    distance(SPIRAL_START, DISTANCE_START, DISTANCE_MIN_FIRST_LOOP, DISTANCE_MAX_FIRST_LOOP, true)
  }

}
