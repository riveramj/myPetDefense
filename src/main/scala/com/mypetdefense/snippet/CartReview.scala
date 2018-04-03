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
      
      val discount = cart.size match {
        case 0 | 1 => 0
        case 2 => subtotal * 0.05
        case _ => subtotal * 0.1
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
        ".cart-pet-remove [onclick]" #> Confirm(s"Remove ${petName}?", ajaxInvoke(removePet(id) _)) &
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

        ".promotion-info [class+]" #> successCoupon &
        "#promo-code" #> ajaxText(couponCode, couponCode = _) &
        ".apply-promo [onClick]" #> SHtml.ajaxInvoke(() => validateCouponCode())
      }
    }
  }
}
