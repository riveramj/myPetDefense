package com.mypetdefense.util

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.UnitTest

class AggregationHelperSpec extends UnitTest {
  import AggregationHelper._

  "combineSimilarItems" should "return input unchanged when items are distinct" in {
    forAllNoShrink(listOfNPosIntsGen()) { inputSrc =>
      val input  = inputSrc.distinct
      val actual = combineSimilarItems(input)(similarity = identity, combine = _ + _)
      actual mustBe input
    }
  }

  it should "combine all items if each of them is similar" in {
    forAllNoShrink(listOfNPosIntsGen()) { input =>
      val actual = combineSimilarItems(input)(similarity = _ => (), combine = _ + _)
      actual mustBe List(input.sum)
    }
  }

  it should "combine items grouped by similarity" in {
    forAllNoShrink(listOfNPosIntsGen()) { input =>
      val (odd, even) = input.partition(_ % 2 != 0)
      val actual      = combineSimilarItems(input)(similarity = _ % 2, combine = _ + _)
      actual.toSet mustBe Set(odd.sum, even.sum).filter(_ != 0)
    }
  }

  it should "keep groups order" in {
    val input    = (25 to 1 by -3).toList
    val actual   = combineSimilarItems(input)(similarity = _ % 5, combine = _ + _)
    val expected = List(35, 29, 23, 17, 13)
    actual mustBe expected
  }

}
