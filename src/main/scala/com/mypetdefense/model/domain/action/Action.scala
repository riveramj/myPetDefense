package com.mypetdefense.model.domain.action

import java.time.Instant

import com.mypetdefense.model.{ActionLog, ActionLogDetails}
import com.mypetdefense.util.DateHelper.nowMillisAsInstant

sealed trait Action extends Product with Serializable {
  val actionType: ActionType
  val actionSubtype: ActionSubtype
  val actionId: Option[Long]
  val userId: Long
  val timestamp: Instant
  def details: Action.Details
}

sealed abstract class CustomerAction(subtype: CustomerActionSubtype) extends Action {
  final override val actionType    = ActionType.CustomerAction
  final override val actionSubtype = subtype
}

sealed abstract class SupportAction(subtype: SupportActionSubtype) extends Action {
  final override val actionType    = ActionType.SupportAction
  final override val actionSubtype = subtype
}

sealed abstract class SystemAction(subtype: SystemActionSubtype) extends Action {
  final override val actionType    = ActionType.SystemAction
  final override val actionSubtype = subtype
}

object Action {
  def apply(
      actionLog: ActionLog,
      longDetails: List[ActionLogDetails],
      stringDetails: List[ActionLogDetails]
  ): Action = {
    val log = ParsedLog(actionLog, longDetails, stringDetails)

    log.actionType match {
      case ActionType.CustomerAction => CustomerAction(log)
      case ActionType.SupportAction  => SupportAction(log)
      case ActionType.SystemAction   => SystemAction(log)
    }
  }

  final case class Details(
      longDetails: Map[String, Long] = Map.empty,
      stringDetails: Map[String, String] = Map.empty
  )
}

object CustomerAction {
  private[action] def apply(log: ParsedLog): CustomerAction =
    log.actionSubtype match {
      case CustomerActionSubtype.CustomerAddedPet   => CustomerAddedPet(log)
      case CustomerActionSubtype.CustomerRemovedPet => CustomerRemovedPet(log)
    }

  final case class CustomerAddedPet(
      userId: Long,
      petId: Long,
      actionId: Option[Long] = None,
      timestamp: Instant = nowMillisAsInstant()
  ) extends CustomerAction(CustomerActionSubtype.CustomerAddedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("petId" -> petId)
      )
  }

  final case class CustomerRemovedPet(
      userId: Long,
      petId: Long,
      actionId: Option[Long] = None,
      timestamp: Instant = nowMillisAsInstant()
  ) extends CustomerAction(CustomerActionSubtype.CustomerRemovedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("petId" -> petId)
      )
  }

  object CustomerAddedPet {
    private[action] def apply(log: ParsedLog): CustomerAddedPet =
      CustomerAddedPet(log.userId, log.longDetails("petId"), Some(log.actionId), log.timestamp)
  }

  object CustomerRemovedPet {
    private[action] def apply(log: ParsedLog): CustomerRemovedPet =
      CustomerRemovedPet(log.userId, log.longDetails("petId"), Some(log.actionId), log.timestamp)
  }
}

object SupportAction {
  private[action] def apply(log: ParsedLog): SupportAction =
    ???

  // TODO
}

object SystemAction {
  private[action] def apply(log: ParsedLog): SystemAction =
    ???

  // TODO
}

private final case class ParsedLog(
    actionType: ActionType,
    actionSubtype: ActionSubtype,
    actionId: Long,
    userId: Long,
    timestamp: Instant,
    longDetails: Map[String, Long],
    stringDetails: Map[String, String]
)

private object ParsedLog {
  def apply(
      actionLog: ActionLog,
      longDetails: List[ActionLogDetails],
      stringDetails: List[ActionLogDetails]
  ): ParsedLog = {
    val actionId   = actionLog.id.get
    val userId     = actionLog.user.get
    val timestamp  = actionLog.timestamp.get.toInstant
    val actionType = ActionType.fromString(actionLog.actionType.get)

    val actionSubtype = actionType match {
      case ActionType.CustomerAction =>
        CustomerActionSubtype.fromString(actionLog.actionSubtype.get)
      case ActionType.SupportAction =>
        SupportActionSubtype.fromString(actionLog.actionSubtype.get)
      case ActionType.SystemAction =>
        SystemActionSubtype.fromString(actionLog.actionSubtype.get)
    }

    val longDetailsMap   = longDetails.map(d => (d.key.get, d.longValue.get)).toMap
    val stringDetailsMap = stringDetails.map(d => (d.key.get, d.stringValue.get)).toMap

    new ParsedLog(
      actionType,
      actionSubtype,
      actionId,
      userId,
      timestamp,
      longDetailsMap,
      stringDetailsMap
    )
  }
}
