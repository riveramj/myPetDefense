package com.mypetdefense.util

import com.mypetdefense.model._
import net.liftweb.common._

object DataLoader extends Loggable {
  def loadProducts = {
    if (Product.findAll().isEmpty) {
      Product.createProduct(
        "Urban Product",
        "For the urban-chic dog",
        5,
        AnimalType.Dog,
        AnimalSize.Medium
      )

      Product.createProduct(
        "Rural Product",
        "For the rural dog",
        5,
        AnimalType.Dog,
        AnimalSize.Small
      )

      Product.createProduct(
        "Outdoor Product",
        "For the adventure-minded cat",
        4,
        AnimalType.Cat,
        AnimalSize.Small
      )

      Product.createProduct(
        "Indoor Product",
        "For the homebody cat",
        4,
        AnimalType.Cat,
        AnimalSize.Large
      )
    }
  }
}
