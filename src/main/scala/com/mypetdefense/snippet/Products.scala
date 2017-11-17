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
  PetFlowChoices.priceCode(Full("default"))

  val products = Product.findAll()

  val prices = Price.findAll(
    By(Price.code, "default"),
    By(Price.active, true)
  )

  def getPriceForProduct(product: String, petType: String) = {
    val price = prices.filter { price =>
      val productName = price.product.obj.map(_.name.get).getOrElse("")
      productName == s"${product} Plus for ${petType}"
    }.headOption.map(_.price.get).getOrElse(0D)

    f"$$$price%2.2f"
  }

  def getImageUrl(product: Box[Product]) = {
    s"images/product-shots/${product.map(_.imageName).openOr("")}"
  }
  
  def getImagePriceRating(productName: String): (String, Double, Double, Int) = {
    val product = products.filter(_.name.get == productName).headOption
    val imageName = s"images/product-shots/${product.map(_.imageName.get).getOrElse("")}"
    val price: Double = product.flatMap { possibleProduct =>
      Price.getDefaultProductPrice(possibleProduct).map(_.price.get)
    }.getOrElse(0D)
    
    val rating = product.map(_.rating.get).getOrElse(0D)
    val reviewCount = product.map(_.reviews.toList.size).getOrElse(0)

    (imageName, price, rating, reviewCount)
  }

  def imagePriceRatingBinding(productName: String) = {
    val (imageName, price, rating, reviewCount) = getImagePriceRating(productName)

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
    ".month-price *" #> f"$$$price%2.2f"
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

    ".frontline-dogs" #> {
      imagePriceRatingBinding("Frontline Plus for Dogs")
    } &
    ".zoguard-dogs" #> {
      imagePriceRatingBinding("ZoGuard Plus for Dogs")
    } &
    ".adventure-dogs" #> {
      imagePriceRatingBinding("Adventure Plus for Dogs")
    } &
    ".shieldtec-dogs" #> {
      imagePriceRatingBinding("ShieldTec Plus for Dogs")
    } &
    ".frontline-cats" #> {
      imagePriceRatingBinding("Frontline Plus for Cats")
    } &
    ".zoguard-cats" #> {
      imagePriceRatingBinding("ZoGuard Plus for Cats")
    } &
    ".adventure-cats" #> {
      imagePriceRatingBinding("Adventure Plus for Cats")
    }
  }
}
