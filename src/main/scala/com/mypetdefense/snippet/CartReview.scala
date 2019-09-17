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

  val priceCode = PetFlowChoices.priceCode.is.openOr(Price.defaultPriceCode)

  var currentPets = completedPets.is

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
    currentPets.remove(petId)
    completedPets(currentPets)
    cartRenderer.map(_.setHtml).openOr(Noop)
  }

  def addNewPet() = {
    petChoice(Empty)
    productChoice(Empty)
    petSize(Empty)
    petId(Empty)

    completedPets(currentPets)

    S.redirectTo(PetChoice.menu.loc.calcDefaultHref)
  }

  def render = {
    val currentPetId = PetFlowChoices.petId.is.openOr(0L)
    
    {
      for {
        petType <- petChoice.is
        size <- petSize.is
        product <- productChoice.is
      } yield {
        val pet = Pet.create
          .petId(currentPetId)
          .animalType(petType)
          .size(size)
          .product(product)

        currentPets(currentPetId) = pet
      }
    }

    SHtml.makeFormsAjax andThen
    "#shopping-cart" #> idMemoize { renderer =>
      cartRenderer = Full(renderer)

      val cart = completedPets.is
      
      val subtotal = {
        for {
          pet <- cart.values
          product <- pet.product.obj
          price <- Price.getPricesByCode(product, priceCode)
        } yield {
          price.price.get
        }
      }.sum
      
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

      val subtotalWithDiscount = subtotal - discount - coupon.map(_.dollarOff.get).openOr(0)

      ".cart-count *" #> cartCount &
      ".starting-total *" #> f"$$$subtotal%2.2f" &
      ".discount *" #> f"-$$$discount%2.2f" &
      ".subtotal *" #> f"$$$subtotalWithDiscount%2.2f" &
      {
        if(coupon.isEmpty) {
          ".free-months" #> ClearNodes &
          ".coupon-discount" #> ClearNodes
        } else {
           if (coupon.map(_.freeMonths.get == 0).openOr(true)) {
            ".free-months" #> ClearNodes &
            ".coupon-discount-amount *" #> f"$$${coupon.map(_.dollarOff.get).openOr(0)}%2.2f"
           } else {
             ".free-months-count *" #> coupon.map(_.freeMonths.get).openOr(0) &
             ".coupon-discount" #> ClearNodes
           }
        }
      } &
      ".cart-item" #> cart.map { case (id, pet) =>
        val petName = pet.name.get
        val product = pet.product.obj
        val productName = product.map(_.name.get).openOr("")
        val productSizeName = product.map(_.getSizeAndSizeName).openOr("")

        val itemPrice = product.flatMap { prod =>
          Price.getPricesByCode(prod, priceCode)
        }.map(_.price.get).openOr(0D)

        ".cart-product-image [src]" #> getImageUrl(product) &
        ".cart-pet-remove [onclick]" #> Confirm(s"Remove ${petName}?", ajaxInvoke(removePet(id) _)) &
        ".product-name-size *" #> s"${productName} ${productSizeName}" &
        ".pet-name" #> ajaxText(petName, possibleName => {
          completedPets(pet.petId.get) = pet.name(possibleName)
          Noop
        }) &
        ".cart-pet-price *" #> f"$$$itemPrice%2.2f"
      } &
      "#add-pet [onClick]" #> ajaxInvoke(addNewPet _) &
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
