package com.mypetdefense.helpers

import com.mypetdefense.helpers.BootUtil.testDatabase
import org.scalatest.{Ignore, Tag}

object TestTags {

  abstract class TestIf(cond: Boolean) extends Tag(if (cond) "" else classOf[Ignore].getName)

  case object PostgresOnlyTest extends TestIf(testDatabase.isPostgreSQL)
  case object H2MemoryOnlyTest extends TestIf(testDatabase.isH2Memory)

}
