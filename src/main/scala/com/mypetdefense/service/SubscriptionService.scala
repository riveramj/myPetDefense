package com.mypetdefense.service

import com.mypetdefense.actor.{EmailActor, UpgradeSubscriptionEmail}
import com.mypetdefense.model._
import com.mypetdefense.service.ParentService.updateStripeSubscriptionQuantity
import com.mypetdefense.util.SecurityContext
import net.liftweb.common.Box
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.mapper.By
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.Props

import java.time.Month

object SubscriptionService {

  def sameDayCancelsByMonth(subscriptions: List[Subscription]): Map[Month, Int] = {
    val sameDayCancels =
      subscriptions
        .filter(_.status.get == Status.Cancelled)
        .filter(_.filterMailedShipments.isEmpty)

    val cancelsByMonth = sameDayCancels.groupBy { subscription =>
      subscription.getCreatedDateOfSubscription.getMonth
    }

    cancelsByMonth.map {
      case (month, subscriptions) =>
        (month, subscriptions.size)
    }
  }

  def upgradeAccount(updatedSubscription: Box[Subscription], admin: Boolean = false) = {
    val boxes = updatedSubscription.map(_.subscriptionBoxes.toList).openOr(Nil)

    for {
      box <- boxes
      if box.animalType.get == AnimalType.Dog
    } yield {
      box.boxType(BoxType.healthAndWellness).saveMe()
      SubscriptionItem.createFirstBox(box)
    }

    val updatedBoxes = boxes.map(_.reload)

    val cost = updatedBoxes.map(SubscriptionBox.possiblePrice(_, true)).sum

    updateStripeSubscriptionQuantity(
      updatedSubscription.map(_.stripeSubscriptionId.get).openOr(""),
      tryo((cost * 100).toInt).openOr(0)
    )

    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! UpgradeSubscriptionEmail(SecurityContext.currentUser, updatedBoxes.size)
    }

    for {
      box          <- updatedBoxes
      subscription <- updatedSubscription.toList
      user         <- SecurityContext.currentUser.toList
      shipmentCount = subscription.shipments.toList.size
    } yield {
      SubscriptionUpgrade.createSubscriptionUpgrade(subscription, box, user, shipmentCount)
      subscription.isUpgraded(true).saveMe()
    }

    if (admin)
      Alert("Account has been upgraded.")
    else
      Alert("Your account has been upgraded! Watch the mail for your new box!")
  }

  def getCurrentSupplements(box: Box[SubscriptionBox]) = {
    for {
      currentBox       <- box.toList
      subscriptionItem <- currentBox.subscriptionItems.toList
      product          <- subscriptionItem.product.obj.toList
      if product.isSupplement.get
    } yield {
      product
    }
  }

  def getAvailableFleaTick(pet: Box[Pet]) = {
    if (pet.map(_.animalType.get.equals(AnimalType.Dog)).openOr(true))
      FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog))
    else
      FleaTick.zoGuardCat.toList
  }

  def getFirstSecondThirdSupplements(supplements: List[Product]) = {
    val firstSupplement = supplements.headOption
    val secondSupplement =
      if (supplements.size > 1) supplements.tail.headOption else None
    val thirdSupplement =
      if (supplements.size > 2) supplements.reverse.headOption else None

    (firstSupplement, secondSupplement, thirdSupplement)
  }

  def getZoGuardProductsAndCurrent(currentFleaTick: Box[FleaTick], availableFleaTick: List[FleaTick]) = {
    if (currentFleaTick.map(_.isZoGuard_?).openOr(false))
      availableFleaTick.filter(_.isZoGuard_?)
    else
      currentFleaTick.toList ++ availableFleaTick.filter(_.isZoGuard_?)
  }

  def saveNewPetProducts(
                          fleaTick: Box[FleaTick],
                          subscriptionBox: Box[SubscriptionBox],
                          supplements: List[Product]
                        ) = {
    for {
      fleaTick <- fleaTick.toList
      box      <- subscriptionBox.toList
      pet      <- box.pet.obj.toList
      _        = box.fleaTick(fleaTick).saveMe
      _        = pet.size(fleaTick.size.get).saveMe
      allItems = box.subscriptionItems.toList
      filteredItems = allItems.filter(supplement =>
        supplement.product.obj.map(_.isSupplement.get).openOr(false)
      )
      index    <- filteredItems.indices
    } yield {
      filteredItems(index).product(supplements(index)).saveMe()
    }
  }
}
