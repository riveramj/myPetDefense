package com.mypetdefense.service 

import net.liftweb._
  import common._
  import mapper._
  import util._
  import util.Helpers._

import com.mypetdefense.model._

object ShipmentService extends Loggable {
  def getCurrentShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate >= CURRENT_DATE and nextShipdate < current_date + interval '5 day'",
        IHaveValidatedThisSQL("mike","2017-04-26")
      )
    )
  }

  def getPendingShipments = {
    Subscription.findAll(
      BySql(
        "nextShipDate > CURRENT_DATE - interval '5 day' and nextshipdate < current_date",
        IHaveValidatedThisSQL("mike","2017-04-26")
      )
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

  def getCurrentPendingPastDueShipments = {
    getCurrentShipments ++ getPendingShipments ++ getPastDueShipments
  }
}
