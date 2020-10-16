package com.mypetdefense.helpers.db

import com.mypetdefense.generator.SubscriptionCreateGeneratedData
import com.mypetdefense.model.{Subscription, User}
import net.liftweb.common.Full

object SubscriptionDbUtils {

  def createSubscription(
      parent: User,
      data: SubscriptionCreateGeneratedData
  ): Subscription = Subscription.createNewSubscription(
    Full(parent),
    data.stripeSubscriptionId,
    data.startDate,
    data.nextShipDate,
    data.priceCode,
    data.isUpgraded,
    data.contractLength
  )

}
