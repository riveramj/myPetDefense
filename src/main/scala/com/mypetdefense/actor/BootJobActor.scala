package com.mypetdefense.actor

import com.mypetdefense.util.{DataLoader, HandlerChain}

sealed trait BootJobMessage
case object MigrateStripeProducts extends BootJobMessage
case object EmailCustomersShutdownCancel extends BootJobMessage

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

trait EmailCustomersShutdownCancelHandling extends HandlerChain {
  addHandler {
    case EmailCustomersShutdownCancel =>
      DataLoader.EmailUsersShutdownCancel
  }
}

object BootJobActor extends BootJobActor
trait BootJobActor
  extends HandlerChain
  with MigrateStripeHandling
  with EmailCustomersShutdownCancelHandling
