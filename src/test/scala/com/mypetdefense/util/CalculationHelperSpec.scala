package com.mypetdefense.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CalculationHelperSpec extends AnyFlatSpec with Matchers {
  it should "calculate occurrences properly" in {
    val inputData    = List(1, 1, 2, 1, 3, 4, 5, 5, 0, 2)
    val expectedData = Map(1 -> 3, 2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 0 -> 1)
    val actualData   = CalculationHelper.calculateOccurrences[Int, Int](inputData, identity)
    actualData shouldBe expectedData
  }

  it should "calculate percentage properly" in {
    val inStart = 3
    val inEnd   = 2
    val result  = CalculationHelper.calculatePercentageDiff(inStart, inEnd).toInt
    result shouldBe -33
  }
}
