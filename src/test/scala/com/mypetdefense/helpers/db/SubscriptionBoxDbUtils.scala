package com.mypetdefense.helpers.db

import com.mypetdefense.model.{BoxType, Pet, Subscription, SubscriptionBox}

object SubscriptionBoxDbUtils {

  def createBox(subscription: Subscription, pet: Pet, boxType: BoxType.Value): SubscriptionBox =
    SubscriptionBox.createNewBox(subscription, pet, boxType)
}
