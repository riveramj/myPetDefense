package com.mypetdefense.helpers

import com.mypetdefense.helpers.GeneralDbUtils.clearTables
import com.mypetdefense.util.DataLoader
import org.scalactic.anyvals.PosInt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.{Locale, TimeZone}

trait DBTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(3)
    )

  override def beforeAll(): Unit = {
    BootUtil.bootOnceForTests
    DataLoader.loadProducts
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
