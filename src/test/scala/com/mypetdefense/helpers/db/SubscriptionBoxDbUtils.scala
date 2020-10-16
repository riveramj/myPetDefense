package com.mypetdefense.helpers.db

import java.util.Date
import com.mypetdefense.model.{Pet, Subscription, SubscriptionBox}

object SubscriptionBoxDbUtils {

  def createBox(subscription: Subscription, pet: Pet): SubscriptionBox =
    SubscriptionBox.createNewBox(subscription, pet)

  def createSubscriptionBoxCreatedAt(subscription: Subscription, pet: Pet, createdAt: Date): SubscriptionBox =
    createBox(subscription, pet).createdAt(createdAt).saveMe()

}
