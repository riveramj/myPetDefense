package com.mypetdefense.model.domain.action

sealed trait ActionType extends Product with Serializable

object ActionType extends EnumCompanion[ActionType] {
  case object CustomerAction extends ActionType
  case object SupportAction  extends ActionType
  case object SystemAction   extends ActionType

  override val values: Set[ActionType] = Set(CustomerAction, SupportAction, SystemAction)
}

sealed trait ActionSubtype         extends Product with Serializable
sealed trait CustomerActionSubtype extends ActionSubtype
sealed trait SupportActionSubtype  extends ActionSubtype
sealed trait SystemActionSubtype   extends ActionSubtype

object CustomerActionSubtype extends EnumCompanion[CustomerActionSubtype] {
  case object CustomerAddedPet   extends CustomerActionSubtype
  case object CustomerRemovedPet extends CustomerActionSubtype

  override val values: Set[CustomerActionSubtype] = Set(CustomerAddedPet, CustomerRemovedPet)
}

object SupportActionSubtype extends EnumCompanion[SupportActionSubtype] {
  // TODO
  override val values: Set[SupportActionSubtype] = Set.empty
}

object SystemActionSubtype extends EnumCompanion[SystemActionSubtype] {
  // TODO
  override val values: Set[SystemActionSubtype] = Set.empty
}

trait EnumCompanion[A] {
  val values: Set[A]
  lazy val fromString: Map[String, A] = values.map(v => (v.toString, v)).toMap
}
