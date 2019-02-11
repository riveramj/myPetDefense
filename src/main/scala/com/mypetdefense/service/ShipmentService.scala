package com.mypetdefense.service 

import net.liftweb._
  import common._
  import mapper._
  import util._
  import util.Helpers._

import com.mypetdefense.model._

object ShipmentService extends Loggable {
  def getCurrentPastDueShipments = {
    Shipment.findAll(
      BySql(
        "expectedShipDate < current_date + interval '23 hour' + interval '55 minutes' and expectedShipDate > current_date - interval '20 day'",
        IHaveValidatedThisSQL("mike","2018-04-24")
      ),
      NullRef(Shipment.dateShipped),
      NotBy(Shipment.status, Status.Cancelled),
      NotBy(Shipment.stripeStatus, Status.Refunded)
    )
  }

  def getUpcomingSubscriptions = {
    Subscription.findAll(
      BySql(
        "nextShipDate > CURRENT_DATE and nextShipDate < CURRENT_DATE + interval '20 day'",
        IHaveValidatedThisSQL("mike","2018-04-24")
      ),
      By(Subscription.status, Status.Active)
    )
  }

  def getPastDueShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate + interval '5 day' < CURRENT_DATE and nextshipdate > current_date - interval '10 day'",
        IHaveValidatedThisSQL("mike","2018-01-04")
      )
    )
  }
}
