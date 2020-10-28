package com.mypetdefense.helpers

import com.mypetdefense.generator.{
  PetChainData,
  PetsAndShipmentChainData,
  ShipmentChainData,
  SubscriptionCreateGeneratedData,
  UserCreateGeneratedData
}
import com.mypetdefense.helpers.db.ShipmentDbUtils.createShipment
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.PetDbUtils.createPet
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.model._

object GeneralDbUtils {

  case class InsertedUserAndSub(user: User, subscription: Subscription)

  case class InsertedUserAndPet(user: User, pets: List[Pet])

  case class InsertedUserSubAndShipment(
      user: User,
      subscription: Subscription,
      shipments: List[Shipment]
  )

  case class InsertedPetsUserSubAndShipment(
      user: User,
      subscription: Subscription,
      shipments: List[Shipment],
      pets: List[Pet]
  )

  def clearTables(): Unit = {
    Address.findAll().map(_.delete_!)
    Agency.findAll().map(_.delete_!)
    Pet.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
    Shipment.findAll().map(_.delete_!)
    User.findAll().map(_.delete_!)
  }

  def insertUserAndSub(
      uIn: UserCreateGeneratedData,
      sIn: SubscriptionCreateGeneratedData
  ): InsertedUserAndSub = {
    val u = createUser(uIn)
    InsertedUserAndSub(u, createSubscription(u, sIn))
  }

  def insertUserSubAndShipment(in: ShipmentChainData): InsertedUserSubAndShipment = {
    val u   = createUser(in.user)
    val su  = createSubscription(u, in.subscriptionCreateGeneratedData)
    val shs = in.shipmentCreateGeneratedData.map(createShipment(u, su, _))
    InsertedUserSubAndShipment(u, su, shs)
  }

  def insertUserAndPet(in: PetChainData): InsertedUserAndPet = {
    val u = createUser(in.user)
    val p = in.petData.map(createPet(u, _))
    InsertedUserAndPet(u, p)
  }

  def insertPetsAndShipmentChainData(
      in: PetsAndShipmentChainData
  ): InsertedPetsUserSubAndShipment = {
    val u    = createUser(in.user)
    val su   = createSubscription(u, in.subscriptionCreateGeneratedData)
    val shs  = in.shipmentCreateGeneratedData.map(createShipment(u, su, _))
    val pets = in.pets.map(createPet(u, _))
    InsertedPetsUserSubAndShipment(u, su, shs, pets)
  }

}
