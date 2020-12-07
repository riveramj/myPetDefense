package com.mypetdefense.helpers

import java.util.{Locale, TimeZone}

import com.mypetdefense.helpers.GeneralDbUtils.clearTables
import com.mypetdefense.util.DataLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait DBTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks {

  override def beforeAll() {
    BootUtil.bootForTests()
    DataLoader.loadProducts

    TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))
    Locale.setDefault(Locale.US)
  }

  override def beforeEach(): Unit = {
    clearTables()
  }

  def cleanUpSuccess(): Assertion = {
    clearTables()
    succeed
  }
}
