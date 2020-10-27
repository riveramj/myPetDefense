package com.mypetdefense.service

import java.time.Month

import com.mypetdefense.model.{Status, Subscription}

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

}
