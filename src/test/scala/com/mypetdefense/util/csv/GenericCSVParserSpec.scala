package com.mypetdefense.util.csv

import com.mypetdefense.helpers.DBTest
import net.liftweb.common.Box
import org.scalatest.Assertion

abstract class GenericCSVParserSpec[R] extends DBTest {

  val parser: GenericCSVParser[R]

  protected implicit class CSVOps(csv: String) {
    def mustBeParsedTo(expectedResult: Box[List[R]]): Assertion =
      parser.parse(csv) mustBe expectedResult

    def checkFirst(assert: R => Assertion): Assertion =
      assert(parser.parse(csv).flatMap(_.headOption).openOrThrowException("parse error"))
  }

}
