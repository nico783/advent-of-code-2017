package adventofcode.day3

import org.scalatest.FunSuite

class Day3Test extends FunSuite {

  test("testSpiralManathanDistance12") {
    assert(Day3.manathanDistanceToSpiralCenter(12) == 3)
  }

  test("testSpiralManathanDistance23") {
    assert(Day3.manathanDistanceToSpiralCenter(23) == 2)
  }

  test("testSpiralManathanDistance1024") {
    assert(Day3.manathanDistanceToSpiralCenter(1024) == 31)
  }

  test("results") {
    println("manathanDistanceToSpiralCenter result: " + Day3.manathanDistanceToSpiralCenter(265149))
  }

}
