package com.mypetdefense.service 

import java.util.Date

import com.mypetdefense.model._
import net.liftweb.common.Box.tryo
import net.liftweb.common.{Full, _}
import net.liftweb.mapper._

object ShipmentService extends Loggable {
  def getCurrentPastDueShipments: List[Shipment] = {
    Shipment.findAll(
      BySql(
        "expectedShipDate < current_date + interval '23 hour' + interval '55 minutes' and expectedShipDate > current_date - interval '20 day'",
        IHaveValidatedThisSQL("mike","2018-04-24")
      ),
      NullRef(Shipment.dateShipped),
      NotBy(Shipment.status, Status.Cancelled),
      By(Shipment.shipmentStatus, ShipmentStatus.Paid)
    )
  }

  def getUpcomingSubscriptions: List[Subscription] = {
    Subscription.findAll(
      BySql(
        "nextShipDate > CURRENT_DATE and nextShipDate < CURRENT_DATE + interval '20 day'",
        IHaveValidatedThisSQL("mike","2018-04-24")
      ),
      By(Subscription.status, Status.Active)
    )
  }

  def getPastDueShipments: List[Subscription] = {
    Subscription.findAll(
      BySql(
        "nextShipDate + interval '5 day' < CURRENT_DATE and nextshipdate > current_date - interval '10 day'",
        IHaveValidatedThisSQL("mike","2018-01-04")
      )
    )
  }

  def createNewShipment(user: User, invoicePaymentId: String, chargeId: Box[String], amountPaid: String, tax: String): Box[Shipment] = {
    for {
      subscription <- user.subscription
      shipmentCount = subscription.shipments.toList.size
      pets = subscription.getPets
      dogs = pets.filter(_.animalType.get == AnimalType.Dog)
    } yield {
      val sendFreeUpgradeShipment =  {
        if (shipmentCount >= 1 && tryo(subscription.freeUpgradeSampleDate) == Full(null) && dogs.nonEmpty)
          true
        else
          false
      }

      if (sendFreeUpgradeShipment)
        subscription.freeUpgradeSampleDate(new Date).saveMe()

      val inserts = Insert.productBrochure.toList ++ {
        if (sendFreeUpgradeShipment)
          Insert.tryUpgrade.toList
        else
          Nil
      }

      Shipment.createShipment(
        user,
        subscription,
        invoicePaymentId,
        chargeId,
        amountPaid,
        tax,
        inserts,
        ShipmentStatus.Paid,
        sendFreeUpgradeShipment
      )
    }
  }
}
