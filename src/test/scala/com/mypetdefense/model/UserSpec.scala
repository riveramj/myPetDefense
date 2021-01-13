package com.mypetdefense.model

import com.mypetdefense.generator.Generator.listOfNUsersGen
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.helpers.DateUtil._
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class UserSpec extends DBTest {

  it should "find yesterday new sales" in {
    forAll(listOfNUsersGen()) { users2Create =>
      val expectedIds =
        users2Create.map(createUser).map(_.createdAt(anyHourOfYesterday).saveMe().id.get)
      val actualIds = User.findYesterdayNewSales.map(_.id.get)

      actualIds should contain theSameElementsAs expectedIds
      cleanUpSuccess()
    }
  }

}
