package com.mypetdefense.helpers

import com.mypetdefense.generator.{
  ShipmentChainData,
  SubscriptionCreateGeneratedData,
  UserCreateGeneratedData
}
import com.mypetdefense.helpers.db.ShipmentDbUtils.createShipment
import com.mypetdefense.helpers.db.SubscriptionDbUtils.createSubscription
import com.mypetdefense.helpers.db.UserDbUtils.createUser
import com.mypetdefense.model._

object GeneralDbUtils {

  case class InsertedUserAndSub(user: User, subscription: Subscription)

  case class InsertedUserSubAndShipment(
      user: User,
      subscription: Subscription,
      shipments: List[Shipment]
  )

  def clearTables(): Unit = {
    User.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
    Address.findAll().map(_.delete_!)
    Pet.findAll().map(_.delete_!)
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

}
