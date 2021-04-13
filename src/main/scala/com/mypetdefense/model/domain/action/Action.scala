package com.mypetdefense.model.domain.action

import com.mypetdefense.model.{ActionLog, ActionLogDetails}
import com.mypetdefense.util.DateHelper.nowMillisAsInstant
import net.liftweb.common.Box.tryo

import java.time.Instant

sealed trait Action extends Product with Serializable {
  val actionType: ActionType
  val actionSubtype: ActionSubtype
  val actionId: Option[Long]
  val userId: Option[Long]
  val parentId: Long
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
      case CustomerActionSubtype.CustomerSignedUp   => CustomerSignedUp(log)
      case CustomerActionSubtype.CustomerRemovedPet => CustomerRemovedPet(log)
      case CustomerActionSubtype.CustomerCanceledAccount => CustomerCanceledAccount(log)
    }

  final case class CustomerSignedUp(
                                     parentId: Long,
                                     userId: Option[Long],
                                     startDate: String,
                                     couponCode: Option[String],
                                     actionId: Option[Long] = None,
                                     timestamp: Instant = nowMillisAsInstant()
                                   ) extends CustomerAction(CustomerActionSubtype.CustomerSignedUp) {
    override def details: Action.Details =
      Action.Details(
        stringDetails = Map("StartDate" -> startDate) ++ couponCode.map("CouponCode" -> _).toMap
      )
  }

  final case class CustomerAddedPet(
                                     parentId: Long,
                                     userId: Option[Long],
                                     petId: Long,
                                     petName: String,
                                     actionId: Option[Long] = None,
                                     timestamp: Instant = nowMillisAsInstant()
  ) extends CustomerAction(CustomerActionSubtype.CustomerAddedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("PetId" -> petId),
        stringDetails = Map("PetName" -> petName)
      )
  }

  final case class CustomerRemovedPet(
                                       parentId: Long,
                                       userId: Option[Long],
                                       petId: Long,
                                       petName: String,
                                       actionId: Option[Long] = None,
                                       timestamp: Instant = nowMillisAsInstant()
  ) extends CustomerAction(CustomerActionSubtype.CustomerRemovedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("PetId" -> petId),
        stringDetails = Map("PetName" -> petName)
      )
  }

  final case class CustomerCanceledAccount(
                                       parentId: Long,
                                       userId: Option[Long],
                                       subscriptionId: Long,
                                       actionId: Option[Long] = None,
                                       timestamp: Instant = nowMillisAsInstant()
                                     ) extends CustomerAction(CustomerActionSubtype.CustomerCanceledAccount) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("SubscriptionId" -> subscriptionId)
      )
  }

  object CustomerAddedPet {
    private[action] def apply(log: ParsedLog): CustomerAddedPet =
      CustomerAddedPet(
        log.parentId,
        log.userId,
        log.longDetails("PetId"),
        log.stringDetails("PetName"),
        Some(log.actionId),
        log.timestamp
      )
  }

  object CustomerRemovedPet {
    private[action] def apply(log: ParsedLog): CustomerRemovedPet =
      CustomerRemovedPet(
        log.parentId,
        log.userId,
        log.longDetails("PetId"),
        log.stringDetails("PetName"),
        Some(log.actionId),
        log.timestamp
      )
  }

  object CustomerCanceledAccount {
    private[action] def apply(log: ParsedLog): CustomerCanceledAccount =
      CustomerCanceledAccount(
        log.parentId,
        log.userId,
        log.longDetails("SubscriptionId"),
        Some(log.actionId),
        log.timestamp
      )
  }

  object CustomerSignedUp {
    private[action] def apply(log: ParsedLog): CustomerSignedUp =
      CustomerSignedUp(
        log.parentId,
        log.userId,
        log.stringDetails("StartDate"),
        tryo(log.stringDetails("CouponCode")),
        Some(log.actionId),
        log.timestamp
      )
  }
}

object SupportAction {
  private[action] def apply(log: ParsedLog): SupportAction =
    log.actionSubtype match {
      case SupportActionSubtype.SupportAddedPet   => SupportAddedPet(log)
      case SupportActionSubtype.SupportRemovedPet => SupportRemovedPet(log)
      case SupportActionSubtype.SupportCanceledAccount => SupportCanceledAccount(log)
    }

  final case class SupportAddedPet(
                                    parentId: Long,
                                    userId: Option[Long],
                                    petId: Long,
                                    petName: String,
                                    actionId: Option[Long] = None,
                                    timestamp: Instant = nowMillisAsInstant()
                                   ) extends SupportAction(SupportActionSubtype.SupportAddedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("PetId" -> petId),
        stringDetails = Map("PetName" -> petName)
      )
  }

  final case class SupportRemovedPet(
                                      parentId: Long,
                                      userId: Option[Long],
                                      petId: Long,
                                      petName: String,
                                      actionId: Option[Long] = None,
                                      timestamp: Instant = nowMillisAsInstant()
                                     ) extends SupportAction(SupportActionSubtype.SupportRemovedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("PetId" -> petId),
        stringDetails = Map("PetName" -> petName)
      )
  }

  final case class SupportCanceledAccount(
                                      parentId: Long,
                                      userId: Option[Long],
                                      subscriptionId: Long,
                                      actionId: Option[Long] = None,
                                      timestamp: Instant = nowMillisAsInstant()
                                    ) extends SupportAction(SupportActionSubtype.SupportCanceledAccount) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("SubscriptionId" -> subscriptionId)
      )
  }

  object SupportAddedPet {
    private[action] def apply(log: ParsedLog): SupportAddedPet =
      SupportAddedPet(log.parentId, log.userId, log.longDetails("PetId"), log.stringDetails("PetName"), Some(log.actionId), log.timestamp)
  }

  object SupportRemovedPet {
    private[action] def apply(log: ParsedLog): SupportRemovedPet =
      SupportRemovedPet(log.parentId, log.userId, log.longDetails("PetId"), log.stringDetails("PetName"), Some(log.actionId), log.timestamp)
  }

  object SupportCanceledAccount {
    private[action] def apply(log: ParsedLog): SupportCanceledAccount =
      SupportCanceledAccount(log.parentId, log.userId, log.longDetails("SubscriptionId"), Some(log.actionId), log.timestamp)
  }
}

object SystemAction {
  private[action] def apply(log: ParsedLog): SystemAction =
    log.actionSubtype match {
      case SystemActionSubtype.SystemRemovedPet => SystemRemovedPet(log)
      case SystemActionSubtype.SystemCanceledAccount => SystemCanceledAccount(log)
    }

  final case class SystemRemovedPet(
                                     parentId: Long,
                                     userId: Option[Long],
                                     petId: Long,
                                     petName: String,
                                     actionId: Option[Long] = None,
                                     timestamp: Instant = nowMillisAsInstant()
                                   ) extends SystemAction(SystemActionSubtype.SystemRemovedPet) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("PetId" -> petId),
        stringDetails = Map("PetName" -> petName)
      )
  }

  final case class SystemCanceledAccount(
                                           parentId: Long,
                                           userId: Option[Long],
                                           subscriptionId: Long,
                                           actionId: Option[Long] = None,
                                           timestamp: Instant = nowMillisAsInstant()
                                         ) extends SystemAction(SystemActionSubtype.SystemCanceledAccount) {
    override def details: Action.Details =
      Action.Details(
        longDetails = Map("SubscriptionId" -> subscriptionId)
      )
  }

  object SystemCanceledAccount {
    private[action] def apply(log: ParsedLog): SystemCanceledAccount =
      SystemCanceledAccount(log.parentId, log.userId, log.longDetails("SubscriptionId"), Some(log.actionId), log.timestamp)
  }

  object SystemRemovedPet {
    private[action] def apply(log: ParsedLog): SystemRemovedPet =
      SystemRemovedPet(log.parentId, log.userId, log.longDetails("PetId"), log.stringDetails("PetName"), Some(log.actionId), log.timestamp)
  }
}

private final case class ParsedLog(
    actionType: ActionType,
    actionSubtype: ActionSubtype,
    actionId: Long,
    userId: Option[Long],
    parentId: Long,
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
    val userId     = Some(actionLog.user.get)
    val parentId   = actionLog.parent.get
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
      parentId,
      timestamp,
      longDetailsMap,
      stringDetailsMap
    )
  }
}
