package com.mypetdefense.service

import java.util.Date

import com.mypetdefense.model.domain.action.{Action, ActionType}
import com.mypetdefense.model.{ActionLog, ActionLogDetails}
import net.liftweb.mapper._

object ActionLogService {

  def logAction(action: Action): Unit = {
    val actionLog =
      ActionLog.create
        .actionType(action.actionType.toString)
        .actionSubtype(action.actionSubtype.toString)
        .user(action.userId)
        .timestamp(Date.from(action.timestamp))
        .saveMe()

    def saveDetails[T](
        details: Map[String, T]
    )(setValue: (ActionLogDetails, T) => ActionLogDetails): Unit =
      details foreach {
        case (key, value) =>
          val detail = ActionLogDetails.create.action(actionLog).key(key)
          setValue(detail, value).saveMe()
      }

    action.details match {
      case Action.Details(longDetails, stringDetails) =>
        saveDetails(longDetails)(_.longValue(_))
        saveDetails(stringDetails)(_.stringValue(_))
    }
  }

  def findActionsByUserIdAndType(userId: Long, actionType: ActionType): List[Action] = {
    val actions =
      ActionLog.findAll(
        By(ActionLog.user, userId),
        By(ActionLog.actionType, actionType.toString)
      )

    val actionIds = actions.map(_.id.get)

    def makeDetails[T](
        valueField: MappedField[T, ActionLogDetails]
    ): ActionLog => List[ActionLogDetails] =
      ActionLogDetails
        .findAll(
          ByList(ActionLogDetails.action, actionIds),
          NotNullRef(valueField)
        )
        .groupBy(_.action.get)
        .withDefaultValue(Nil)
        .compose[ActionLog](_.id.get)

    val longDetails   = makeDetails(ActionLogDetails.longValue)
    val stringDetails = makeDetails(ActionLogDetails.stringValue)

    actions.map(a => Action(a, longDetails(a), stringDetails(a)))
  }

}
