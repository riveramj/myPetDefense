package com.mypetdefense.snippet 

import net.liftweb._
  import common._
  import mapper._
  import http._
    import LiftRules._
    import rest._
    import js._
      import JE._
      import JsExp._
  import util._
    import Helpers._
  import json._
    import Extraction._
    import JsonDSL._

import com.mypetdefense.model._

import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

import java.util.Date
import java.time.{LocalDate, ZoneId}

import dispatch.{Req => DispatchReq, _}, Defaults._

case class FriendsFamilyItem(sku: String, quantity: Int)

object FriendsFamilyAPI extends RestHelper with Loggable {
  serve {
    case req @ Req("authy" :: Nil, _, PostRequest) => {
      println("----1")
      println(req.hostAndPath)
      println(req.headers)
      val body = req.body
      println(body.map(reqBody => tryo(Serialization.read[JValue](new String(reqBody)))))
      println("----5")

      OkResponse()
    }
    
    case req @ Req("api" :: "v1" :: "orders" :: Nil, _, PostRequest) => {
      for {
        requestBody <- (req.body ?~ "No request body." ~> 400)
        requestJson <- tryo(Serialization.read[JValue](new String(requestBody))) ?~! "Invalid JSON." ~> 400
        name <- tryo((requestJson \ "name").extract[String]) ?~! "Name is missing." ~> 400
        email <- tryo((requestJson \ "email").extract[String]) ?~! "Email is missing." ~> 400
        stripeOrderId <- tryo((requestJson \ "stripeOrderId").extract[String]) ?~! "stripeOrderId is missing." ~> 400
        tax <- tryo((requestJson \ "tax").extract[Double]) ?~! "Tax is missing." ~> 400
        total <- tryo((requestJson \ "total").extract[Double]) ?~! "Total is missing." ~> 400
        shippingJson <- Full(requestJson \ "shipping")
        shipping <- tryo(shippingJson.extract[NewAddress]) ?~ "Error in Shipping JSON." ~> 400
        productsJson <- Full(requestJson \ "products")
        possibleProducts <- tryo(productsJson.extract[List[FriendsFamilyItem]]) ?~ "Error in items JSON." ~> 400
      } yield {


        val products = possibleProducts.map { product =>
          FriendsFamilyProduct.find(By(FriendsFamilyProduct.sku, product.sku)).map((product.quantity, _))
        }.flatten

        val realSkus = products.map(_._2)

        if (products.size != possibleProducts.size) {

          val missingSku = possibleProducts.map(_.sku).diff(realSkus.map(_.sku.get))

          JsonResponse(
            ("missing sku" -> missingSku),
            Nil,
            Nil,
            404
          )
        } else {
          val order = FriendsFamilyOrder.createOrder(
            name,
            email,
            stripeOrderId,
            shipping,
            total,
            tax,
            products
          )

          JsonResponse(
            ("type" -> "Order") ~ ("id" -> order.orderId.get),
            Nil,
            Nil,
            201
          )
        }
      }
    }
  }
}
