package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb._
  import http.SHtml._
  import util._
  import util.Helpers._
  import common._
  import util.ClearClearable
  import http._
  import mapper.{By, NullRef}
  import js._
      import JsCmds._

import com.mypetdefense.service._
  import ValidationService._
  import PetFlowChoices._

import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.model._
import com.mypetdefense.actor._

object Products extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Products") / "products"
}

class Products extends Loggable {
  val currentCoupon = PetFlowChoices.coupon.is
  val currentCouponCode = currentCoupon.map(_.couponCode.get).openOr("")
  val priceCode = PetFlowChoices.priceCode.is.openOr("default")

  val products = Product.findAll()

  val prices = Price.findAll(
    By(Price.code, priceCode),
    By(Price.active, true)
  )

  val defaultPrices = Price.findAll(
    By(Price.code, "default"),
    By(Price.active, true)
  )

  def getPriceForProduct(product: Product, priceList: List[Price]) = {
    priceList.filter { price =>
      price.product.obj == product
    }.headOption.map(_.price.get).getOrElse(0D)
  }

  def getImageUrl(product: Box[Product]) = {
    s"images/product-shots/${product.map(_.imageName).openOr("")}"
  }
  
  def getImagePriceRating(productName: String): (String, Double, Double, Double, Int) = {
    val product = products.filter(_.name.get == productName).headOption
    val imageName = s"images/product-shots/${product.map(_.imageName.get).getOrElse("")}"
    
    val price = product.map { possibleProduct =>
      getPriceForProduct(possibleProduct, defaultPrices)
    }.getOrElse(0D)

    val salePrice: Double = if (priceCode == "cold5k") {
      product.map { possibleProduct =>
        getPriceForProduct(possibleProduct, prices)
      }.getOrElse(0D)
    } else {
      0D
    }
    
    val rating = product.map(_.rating.get).getOrElse(0D)
    val reviewCount = product.map(_.reviews.toList.size).getOrElse(0)

    (imageName, price, salePrice, rating, reviewCount)
  }

  def imagePriceRatingBinding(productName: String) = {
    val (imageName, defaultPrice, salePrice, rating, reviewCount) = getImagePriceRating(productName)

    val starBinding = rating match {
      case 0D => 
        ".star [class+]" #> "empty"

      case star if star < 2D =>
        ".one [class+]" #> "filled" &
        ".two [class+]" #> "empty" &
        ".three [class+]" #> "empty" &
        ".four [class+]" #> "empty" &
        ".five [class+]" #> "empty"

      case star if star < 3D =>
        ".one [class+]" #> "filled" &
        ".two [class+]" #> "filled" &
        ".three [class+]" #> "empty" &
        ".four [class+]" #> "empty" &
        ".five [class+]" #> "empty"

      case star if star < 3.5D =>
        ".one [class+]" #> "filled" &
        ".two [class+]" #> "filled" &
        ".three [class+]" #> "filled" &
        ".four [class+]" #> "empty" &
        ".five [class+]" #> "empty"

      case star if star < 4D =>
        ".one [class+]" #> "filled" &
        ".two [class+]" #> "filled" &
        ".three [class+]" #> "filled" &
        ".four [class+]" #> "half" &
        ".five [class+]" #> "empty"

      case star if star < 4.5D =>
        ".one [class+]" #> "filled" &
        ".two [class+]" #> "filled" &
        ".three [class+]" #> "filled" &
        ".four [class+]" #> "filled" &
        ".five [class+]" #> "empty"

      case star if star < 5D =>
        ".one [class+]" #> "filled" &
        ".two [class+]" #> "filled" &
        ".three [class+]" #> "filled" &
        ".four [class+]" #> "filled" &
        ".five [class+]" #> "half"

      case star if star == 5D =>
        ".star [class+]" #> "filled"

      case star =>
        ".star [class+]" #> "empty"
    }

    ".rating [title]" #> f"Average Rating: $rating%1.2f" &
    starBinding &
    ".count *" #> reviewCount &
    ".product-shot img [src]" #> imageName &
    ".month-price *" #> f"$$$defaultPrice%2.2f" &
    ".sale-price *" #> f"$$$salePrice%2.2f"
  }

  def render = {
    if (PetFlowChoices.purchased.is.openOr(false)) {
      PetFlowChoices.total(Empty)
      PetFlowChoices.freeMonths(Empty)
      PetFlowChoices.shoppingCart(Map())
      PetFlowChoices.coupon(Empty)
      PetFlowChoices.recentProduct(Empty)
      PetFlowChoices.groupons(Nil)
      PetFlowChoices.subtotal(Empty)
      PetFlowChoices.discount(Empty)
      PetFlowChoices.purchased(Empty)
      PetFlowChoices.shoppingCart(Map())
    }

    ".products .frontline-dogs" #> {
      imagePriceRatingBinding("Frontline Plus for Dogs")
    } &
    ".products .zoguard-dogs" #> {
      imagePriceRatingBinding("ZoGuard Plus for Dogs")
    } &
    ".products .adventure-dogs" #> {
      imagePriceRatingBinding("Adventure Plus for Dogs")
    } &
    ".products .shieldtec-dogs" #> {
      imagePriceRatingBinding("ShieldTec Plus for Dogs")
    } &
    ".products .frontline-cats" #> {
      imagePriceRatingBinding("Frontline Plus for Cats")
    } &
    ".products .zoguard-cats" #> {
      imagePriceRatingBinding("ZoGuard Plus for Cats")
    } &
    ".products .adventure-cats" #> {
      imagePriceRatingBinding("Adventure Plus for Cats")
    } andThen
    {
      if (currentCouponCode == "cold5k") {
        ".products [class+]" #> "sale" &
        ".generic-price [class+]" #> "strike"
      } else {
        ".sale-price" #> ClearNodes
      }
    }
  }
}
