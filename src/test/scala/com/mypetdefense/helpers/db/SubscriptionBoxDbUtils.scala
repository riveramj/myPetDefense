package com.mypetdefense.helpers.db

import com.mypetdefense.model.{Pet, Subscription, SubscriptionBox}

object SubscriptionBoxDbUtils {

  def createBox(subscription: Subscription, pet: Pet): SubscriptionBox =
    SubscriptionBox.createNewBox(subscription, pet)
}
