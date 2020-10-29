package com.mypetdefense.helpers

import com.mypetdefense.generator._
import com.mypetdefense.helpers.db.ShipmentDbUtils.createShipment
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.PetDbUtils.createPet
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.model._
import net.liftweb.common._

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
    Event.findAll().map(_.delete_!)
    Pet.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
    SubscriptionBox.findAll().map(_.delete_!)
    Shipment.findAll().map(_.delete_!)
    User.findAll().map(_.delete_!)
  }

  def insertUserAndSub(
      uIn: UserCreateGeneratedData,
      sIn: SubscriptionCreateGeneratedData
  ): InsertedUserAndSub = {
    val u = createUser(uIn)
    InsertedUserAndSub(u, createSubscription(Full(u), sIn))
  }

  def insertSubWithoutUser(sIn: SubscriptionCreateGeneratedData): Subscription =
    createSubscription(Empty, sIn)

  def insertUserSubAndShipment(in: ShipmentChainData): InsertedUserSubAndShipment = {
    val u        = createUser(in.user)
    val su       = createSubscription(Full(u), in.subscriptionCreateGeneratedData)
    val updatedU = u.subscription(su).saveMe()
    val shs      = in.shipmentCreateGeneratedData.map(createShipment(u, su, _))
    InsertedUserSubAndShipment(updatedU, su, shs)
  }

  def insertUserAndPet(in: PetChainData): InsertedUserAndPet = {
    val u = createUser(in.user)
    val p = in.petData.map(createPet(u, _))
    InsertedUserAndPet(u, p)
  }

  def insertPetsAndShipmentChainData(
      in: PetsAndShipmentChainData
  ): InsertedPetsUserSubAndShipment = {
    val u   = createUser(in.user)
    val su  = createSubscription(Full(u), in.subscriptionCreateGeneratedData)
    val shs = in.shipmentCreateGeneratedData.map(createShipment(u, su, _))
    val pets = in.pets.map { pData =>
      val createdPet = createPet(u, pData)
      SubscriptionBox.createNewBox(su, createdPet)
      createdPet
    }
    InsertedPetsUserSubAndShipment(u, su.refresh.toOption.get, shs, pets)
  }

}
