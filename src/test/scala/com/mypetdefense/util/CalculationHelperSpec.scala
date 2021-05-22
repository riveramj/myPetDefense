package com.mypetdefense.util

import com.mypetdefense.helpers.UnitTest

class CalculationHelperSpec extends UnitTest {
  it should "calculate occurrences properly" in {
    val inputData    = List(1, 1, 2, 1, 3, 4, 5, 5, 0, 2)
    val expectedData = Map(1 -> 3, 2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 0 -> 1)
    val actualData   = CalculationHelper.countOccurrences(inputData)
    actualData mustBe expectedData
  }

  it should "calculate percentage properly" in {
    val inStart = 3
    val inEnd   = 2
    val result  = CalculationHelper.calculatePercentageDiff(inStart, inEnd).toInt
    result mustBe -33
  }
}
