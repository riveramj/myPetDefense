package com.mypetdefense.helpers.db

import com.mypetdefense.generator.SubscriptionUpgradeCreateGeneratedData
import com.mypetdefense.model._

object SubscriptionUpgradeDbUtil {
  def createSubscriptionUpgrade(
    in: SubscriptionUpgradeCreateGeneratedData,
    user: User,
    subscription: Subscription
  ): SubscriptionUpgrade = {
    SubscriptionUpgrade.createSubscriptionUpgrade(
      subscription,
      user,
      in.shipmentCountAtUpgrade
    )
  }
}
