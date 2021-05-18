package com.mypetdefense.actor

import com.mypetdefense.util.{DataLoader, HandlerChain}

sealed trait BootJobMessage
case object MigrateStripeProducts extends BootJobMessage

trait MigrateStripeHandling extends HandlerChain {
  DataLoader.createStripeProductsPrices
  DataLoader.createFiveDollarPrices
  DataLoader.createChangeProduct
  DataLoader.migrateToStripeProducts  
}

object BootJobActor extends BootJobActor
trait BootJobActor
  extends HandlerChain
  with MigrateStripeHandling
