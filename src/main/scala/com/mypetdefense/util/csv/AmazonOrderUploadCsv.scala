package com.mypetdefense.util.csv

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime}
import java.util.Date

import com.mypetdefense.model._
import net.liftweb.common._

object AmazonOrderUploadCsv extends GenericCSVParser[AmazonOrder] {
  override def convertRowToModel(
      fieldList: Array[String],
      lineCount: Int,
      headerIndex: Map[Columns.Value, Int]
  ): Box[AmazonOrder] = {
    import Columns._

    val amazonOrderId     = cellValue(AmazonOrderId, headerIndex, fieldList).openOr("")
    val buyerEmail        = cellValue(BuyerEmail, headerIndex, fieldList).openOr("")
    val buyerPhone        = cellValue(BuyerPhone, headerIndex, fieldList).openOr("")
    val productName       = cellValue(ProductName, headerIndex, fieldList).openOr("")
    val quantityPurchased = cellInt(QuantityPurchased, headerIndex, fieldList).openOr(0)
    val productPrice      = cellDouble(ProductPrice, headerIndex, fieldList).openOr(0d)
    val productDiscount   = cellDouble(ProductDiscount, headerIndex, fieldList).openOr(0d)
    val carrier           = cellValue(Carrier, headerIndex, fieldList).openOr("")
    val sku               = cellValue(Sku, headerIndex, fieldList).openOr("")
    val rawPurchaseDate   = cellValue(PurchaseDate, headerIndex, fieldList).openOr("")
    val name              = cellValue(RecipientName, headerIndex, fieldList).openOr("")
    val address1          = cellValue(ShipAddress1, headerIndex, fieldList).openOr("")
    val address2          = cellValue(ShipAddress2, headerIndex, fieldList).openOr("")
    val address3          = cellValue(ShipAddress3, headerIndex, fieldList).openOr("")
    val city              = cellValue(ShipCity, headerIndex, fieldList).openOr("")
    val state             = cellValue(ShipState, headerIndex, fieldList).openOr("")
    val zip               = cellValue(ShipZip, headerIndex, fieldList).openOr("")

    val timeFormatter = DateTimeFormatter.ISO_DATE_TIME
    val offsetDate    = OffsetDateTime.parse(rawPurchaseDate, timeFormatter)
    val purchaseDate  = Date.from(Instant.from(offsetDate))

    val animalType = {
      if (productName.toLowerCase().contains("dogs"))
        AnimalType.Dog
      else
        AnimalType.Cat
    }

    Full(
      AmazonOrder.create
        .amazonOrderId(amazonOrderId)
        .email(buyerEmail)
        .phone(buyerPhone)
        .quantityPurchased(quantityPurchased)
        .productPrice(productPrice)
        .productDiscount(productDiscount)
        .carrier(carrier)
        .productName(productName)
        .sku(sku)
        .name(name)
        .address1(address1)
        .address2(address2)
        .address3(address3)
        .city(city)
        .state(state)
        .zip(zip)
        .animalType(animalType)
        .purchaseDate(purchaseDate)
    )
  }

  object Columns extends Columns {
    val AmazonOrderId: HeaderValue     = requiredValue("amazon-order-id")
    val BuyerEmail: HeaderValue        = requiredValue("buyer-email")
    val BuyerPhone: HeaderValue        = requiredValue("buyer-phone-number")
    val ProductName: HeaderValue       = requiredValue("product-name")
    val QuantityPurchased: HeaderValue = requiredValue("quantity-shipped")
    val ProductPrice: HeaderValue      = requiredValue("item-price")
    val ProductDiscount: HeaderValue   = requiredValue("item-promotion-discount")
    val Carrier: HeaderValue           = requiredValue("carrier")
    val Sku: HeaderValue               = requiredValue("sku")
    val PurchaseDate: HeaderValue      = requiredValue("purchase-date")
    val RecipientName: HeaderValue     = requiredValue("recipient-name")
    val ShipAddress1: HeaderValue      = requiredValue("ship-address-1")
    val ShipAddress2: HeaderValue      = requiredValue("ship-address-2")
    val ShipAddress3: HeaderValue      = requiredValue("ship-address-3")
    val ShipCity: HeaderValue          = requiredValue("ship-city")
    val ShipState: HeaderValue         = requiredValue("ship-state")
    val ShipZip: HeaderValue           = requiredValue("ship-postal-code")
  }
}
