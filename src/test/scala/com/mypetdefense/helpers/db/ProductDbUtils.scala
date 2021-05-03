package com.mypetdefense.helpers.db

import com.mypetdefense.generator.ProductGeneratedData
import com.mypetdefense.model.{AnimalType, Product}

object ProductDbUtils {
  def createNewProduct(productData: ProductGeneratedData): Product =
    Product.createNewProduct(productData.name, productData.sku, productData.quantity, AnimalType.Dog, productData.isSupplement)
}
