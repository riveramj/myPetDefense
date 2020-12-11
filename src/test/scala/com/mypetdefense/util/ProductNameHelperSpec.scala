package com.mypetdefense.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ProductNameHelperSpec extends AnyFlatSpec with Matchers {

  it should "sanitize name properly" in {
    val expectedZooGuard  = "5-22 x"
    val expectedAdventure = "3-10 y"
    val expectedShield    = "5-15 z"

    val result = ProductNameHelper.sanitizeFleaTickNames(
      List(expectedZooGuard, expectedAdventure, expectedShield)
    )

    result should contain theSameElementsInOrderAs List(
      "ZoGuard Plus for Dogs 05-22 lbs",
      "Adventure Plus for Dogs, 3-10 lbs",
      "ShieldTec Plus for Dogs, 05-15 lbs"
    )
  }

}
