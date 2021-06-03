package com.mypetdefense.actor

import com.mypetdefense.model.{BoxType, Price}
import com.mypetdefense.util.{DataLoader, HandlerChain}
import net.liftweb.mapper.By

sealed trait BootJobMessage
case object MigrateStripeProducts extends BootJobMessage
case object CreateEverydayBox extends BootJobMessage

trait MigrateStripeHandling extends HandlerChain {
  addHandler {
    case MigrateStripeProducts =>
      //DataLoader.createStripeProductsPrices
      //DataLoader.createFiveDollarPrices
      //DataLoader.createChangeProduct
      //DataLoader.addPetSizeToPrice
      DataLoader.migrateToStripeProducts
  }
}

trait CreateEverydayBoxHandling extends HandlerChain {
  addHandler {
    case CreateEverydayBox =>
      if (Price.findAll(By(Price.boxType, BoxType.everydayWellness)).isEmpty)
        DataLoader.createEverydayStripeProductsPrices
  }
}

object BootJobActor extends BootJobActor
trait BootJobActor
  extends HandlerChain
  with MigrateStripeHandling
  with CreateEverydayBoxHandling
