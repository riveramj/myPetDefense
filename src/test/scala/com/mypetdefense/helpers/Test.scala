package com.mypetdefense.helpers

import com.mypetdefense.helpers.GeneralDbUtils.clearTables
import com.mypetdefense.util.DataLoader
import net.liftweb.common.Empty
import net.liftweb.http.{LiftSession, S}
import net.liftweb.util.StringHelpers
import org.scalactic.anyvals.PosInt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach, Outcome}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.{Locale, TimeZone}

sealed trait AnyTest extends AnyFlatSpec with must.Matchers with ScalaCheckPropertyChecks

trait UnitTest extends AnyTest {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(10)
    )
}

trait IntegrationTest extends AnyTest {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(3)
    )
}

trait DBTest extends IntegrationTest with BeforeAndAfterEach with BeforeAndAfterAll {
  override def beforeAll(): Unit = {
    BootUtil.bootOnceForTests
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

trait LiftTest extends DBTest {
  val session = new LiftSession("", StringHelpers.randomString(10), Empty)

  override protected def withFixture(test: NoArgTest): Outcome = {
    S.initIfUninitted(session) {
      test()
    }
  }
}
