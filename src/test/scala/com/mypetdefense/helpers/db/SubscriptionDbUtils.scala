package com.mypetdefense.helpers.db

import com.mypetdefense.generator.SubscriptionCreateGeneratedData
import com.mypetdefense.model.{Subscription, User}
import net.liftweb.common.Box

object SubscriptionDbUtils {

  def createSubscription(
      parent: Box[User],
      data: SubscriptionCreateGeneratedData
  ): Subscription = Subscription.createNewSubscription(
    parent,
    data.stripeSubscriptionId,
    data.startDate,
    data.nextShipDate,
    data.priceCode,
    contractLength = data.contractLength
  )

}
