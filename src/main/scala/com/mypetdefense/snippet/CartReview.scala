 package com.mypetdefense.snippet

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

import com.mypetdefense.model._

import java.util.Date
import java.text.SimpleDateFormat

object CartReview extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Review Cart") / "cart-review"
}

class CartReview extends Loggable {
  import PetFlowChoices._

  var cartRenderer: Box[IdMemoizeTransform] = Empty
  
  var coupon: Box[Coupon] = PetFlowChoices.coupon.is
  var couponCode = coupon.map(_.couponCode.get).openOr("")

  var groupons: List[Groupon] = PetFlowChoices.groupons.is
  var grouponCodes = groupons.map(_.grouponCode.get)
  var possibleGrouponCode = ""

  def getImageUrl(product: Box[Product]) = {
    s"images/product-shots/${product.map(_.imageName).openOr("")}"
  }

  def validateCouponCode() = {
    val possibleCoupon = Coupon.find(By(Coupon.couponCode, couponCode.toLowerCase()))

    if (possibleCoupon.isEmpty) {
      PromoCodeMessage("error")
    } else {
      coupon = possibleCoupon
      PetFlowChoices.coupon(coupon)

      (
        PromoCodeMessage("success") &
        cartRenderer.map(_.setHtml).openOr(Noop)
      )
    }
  }

  def validateGrouponCode() = {
    def shoppingCartDoesNotQualify = {
      val cart = PetFlowChoices.shoppingCart.is

      val productNames = cart.map(_._2._2.name.get)

      val numberOfNonFrontlines = productNames.count { name =>
        (name != "Frontline Plus for Dogs") && (name != "Frontline Plus for Cats")
      }

      println(productNames + " === " + numberOfNonFrontlines)
      println(!(numberOfNonFrontlines >= (groupons.size + 1)) + " ===")

      (groupons.size + 1) > numberOfNonFrontlines
    }

    val possibleGroupon = Groupon.find(By(Groupon.grouponCode, possibleGrouponCode.toLowerCase()))

    if (possibleGroupon.isEmpty || possibleGrouponCode.isEmpty) {
      GrouponCodeMessage("error") 
    } else if (shoppingCartDoesNotQualify) {
      GrouponCodeMessage("frontline-error")
    } else {
      groupons = (groupons ++ possibleGroupon.toList).distinct
      PetFlowChoices.groupons(groupons)
      PetFlowChoices.freeMonths(groupons.headOption.map(_.freeMonths.get))

      (
        GrouponCodeMessage("success") &
        cartRenderer.map(_.setHtml).openOr(Noop)
      )
    }
  }

  def removePet(petId: Long)() = {
    shoppingCart(shoppingCart.is - petId)
    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def render = {
    SHtml.makeFormsAjax andThen
    "#shopping-cart" #> idMemoize { renderer =>
      cartRenderer = Full(renderer)

      val cart = shoppingCart.is
      val subtotal = cart.values.map(_._3).sum
      
      val multiPetDiscount = cart.size match {
        case 0 | 1 => 0
        case 2 => subtotal * 0.05
        case _ => subtotal * 0.1
      }

      val grouponDiscount = groupons.size * 12.99

      val discount = {
        if (grouponDiscount > 0) 
          grouponDiscount
        else
          multiPetDiscount
      }

      PetFlowChoices.discount(Full(discount))
      PetFlowChoices.subtotal(Full(subtotal))

      val cartCount = cart.size match {
        case 1 => s"1 item"
        case count => s"${count} items"
      }

      val subtotalWithDiscount = subtotal - discount

      ".cart-count *" #> cartCount &
      ".starting-total *" #> f"$$$subtotal%2.2f" &
      ".discount *" #> f"-$$$discount%2.2f" &
      ".subtotal *" #> f"$$$subtotalWithDiscount%2.2f" &
      {
        if(coupon.isEmpty) {
          ".free-months" #> ClearNodes
        } else {
          ".free-months-count *" #> coupon.map(_.freeMonths.get).openOr(0)
        }
      } &
      ".cart-item" #> cart.map { case (id, pet) =>
        val petName = pet._1
        val product = pet._2
        val itemPrice = pet._3

        ".cart-product-image [src]" #> getImageUrl(Full(product)) &
        ".cart-pet-remove [onclick]" #> Confirm(s"Remove ${petName}?", ajaxInvoke(removePet(id))) &
        ".product-name-size *" #> s"${product.name.get} ${product.getSizeAndSizeName}" &
        ".pet-name" #> ajaxText(petName, possibleName => {
          shoppingCart(shoppingCart.is + (id -> pet.copy(_1 = possibleName)))
          Noop
        }) &
        ".cart-pet-price *" #> f"$$$itemPrice%2.2f"
      } &
      {
        val successCoupon = {
          if (!PetFlowChoices.coupon.isEmpty)
            "promo-success"
          else
            ""
        }

        val successGroupon = {
          if (!PetFlowChoices.groupons.isEmpty)
            "groupon-success"
          else
            ""
        }

        ".promotion-info [class+]" #> successCoupon &
        ".groupon-info [class+]" #> successGroupon &
        "#promo-code" #> ajaxText(couponCode, couponCode = _) &
        "#groupon-code" #> ajaxText(possibleGrouponCode, possibleGrouponCode = _) &
        ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode()) &
        ".apply-groupon [onClick]" #> SHtml.ajaxInvoke(() => validateGrouponCode()) &
        ".groupon" #> groupons.map { groupon =>
          ".code *" #> groupon.grouponCode.get
        }
      }
    }
  }
}
