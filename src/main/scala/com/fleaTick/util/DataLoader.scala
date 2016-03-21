package com.fleaTick.util

import com.fleaTick.model._
import net.liftweb.common._

object DataLoader extends Loggable {
  def loadProducts = {
    if (Product.findAll().isEmpty) {
      Product.createProduct(
        "Urban Product",
        "For the urban-chic dog",
        5,
        AnimalType.Dog,
        AnimalSize.Medium,
        SubscriptionType.Month
      )

      Product.createProduct(
        "Rural Product",
        "For the rural dog",
        5,
        AnimalType.Dog,
        AnimalSize.Small,
        SubscriptionType.Month
      )

      Product.createProduct(
        "Outdoor Product",
        "For the adventure-minded cat",
        4,
        AnimalType.Cat,
        AnimalSize.Small,
        SubscriptionType.Month
      )

      Product.createProduct(
        "Indoor Product",
        "For the homebody cat",
        4,
        AnimalType.Cat,
        AnimalSize.Small,
        SubscriptionType.Month
      )
    }
  }
}
