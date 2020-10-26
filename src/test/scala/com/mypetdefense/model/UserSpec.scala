package com.mypetdefense.model

import com.mypetdefense.generator.Generator.listOfNUsersGen
import com.mypetdefense.helpers.BootUtil
import com.mypetdefense.helpers.DateUtil.{ZonedDateTimeSyntax, anyHourOfYesterday}
import com.mypetdefense.helpers.GeneralDbUtils.clearTables
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class UserSpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks {

  override def beforeAll() {
    BootUtil.bootForTests()
  }

  override def afterAll(): Unit = {
    clearTables()
  }

  override def afterEach(): Unit = {
    clearTables()
  }

  private def cleanUpSuccess(): Assertion = {
    clearTables()
    succeed
  }

  it should "find yesterday new sales" in {
    forAll(listOfNUsersGen()) { users2Create =>
      val expectedIds =
        users2Create.map(createUser).map(_.createdAt(anyHourOfYesterday.toDate).saveMe().id.get)
      val actualIds = User.findYesterdayNewSales.map(_.id.get)

      actualIds should contain theSameElementsAs expectedIds
      cleanUpSuccess()
    }
  }

}
