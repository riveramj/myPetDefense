package com.mypetdefense.service

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.model.domain.action.ActionSubtype
import com.mypetdefense.model.{ActionLog, ActionLogDetails}

class ActionLogServiceSpec extends DBTest {

  "ActionLogService" should "save common action info to the ActionLog table" in {
    forAllNoShrink(genCustomerAction) { action =>
      ActionLogService.logAction(action)

      val actionLogs = ActionLog.findAll()
      actionLogs.length mustBe 1

      val log = actionLogs.head
      log.actionType.get mustBe action.actionType.toString
      log.actionSubtype.get mustBe action.actionSubtype.toString
      log.user.get mustBe action.userId.getOrElse(0)
      log.parent.get mustBe action.parentId
      log.timestamp.get.toInstant mustBe action.timestamp

      cleanUpSuccess()
    }
  }

  it should "save action-specific info to the ActionLogDetails table" in {
    forAllNoShrink(genCustomerAction) { action =>
      ActionLogService.logAction(action)

      val details = ActionLogDetails.findAll()
      details.length mustBe 2

      val detail = details.head
      detail.key.get mustBe "PetId"
      detail.longValue.get mustBe action.details.longDetails("PetId")
      detail.stringValue.get mustBe ""

      cleanUpSuccess()
    }
  }

  it should "log any action and be able to reconstruct it from DB" in {
    forAllNoShrink(genCustomerAction) { action =>
      ActionLogService.logAction(action)

      val result = ActionLogService.findActionsByParentId(action.parentId)
      result.length mustBe 1

      val actual = result.head
      actual.getClass mustBe action.getClass
      actual.actionSubtype mustBe action.actionSubtype
      actual.actionId mustBe defined
      actual.userId mustBe action.userId
      actual.timestamp mustBe action.timestamp
      actual.details mustBe action.details

      cleanUpSuccess()
    }
  }

  it should "find all logged actions" in {
    forAllNoShrink(listOfNCustomerActions()) { actions =>
      actions foreach { action => ActionLogService.logAction(action) }

      def expected(parentId: Long): List[(ActionSubtype, Long)] =
        actions
          .filter(_.parentId == parentId)
          .map(a => (a.actionSubtype, a.details.longDetails("PetId")))

      def actual(parentId: Long): List[(ActionSubtype, Long)] =
        ActionLogService
          .findActionsByParentId(parentId)
          .map(a => (a.actionSubtype, a.details.longDetails("PetId")))

      actual(parentId = 1L) mustBe expected(parentId = 1L)
      actual(parentId = 2L) mustBe expected(parentId = 2L)

      cleanUpSuccess()
    }
  }

}
