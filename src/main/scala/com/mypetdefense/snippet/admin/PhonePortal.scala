package com.mypetdefense.snippet
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service._
  import ValidationService._

object PhonePortal extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Phone Portal") / "admin" / "phone-portal"
}

class PhonePortal extends Loggable {

  var petType: Box[AnimalType.Value] = Empty
  var chosenProduct: Box[Product] = Empty

  var email = ""
  var petName = ""
  
  var firstName = ""
  var cardholderName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  var taxRate = 0D
  var taxDue = 0D
  var priceAdditionsRenderer: Box[IdMemoizeTransform] = None
  var orderSummaryRenderer: Box[IdMemoizeTransform] = None

  var stripeToken = ""
  var couponCode = ""
  var coupon: Box[Coupon] = None

  def petTypeRadio(renderer: IdMemoizeTransform) = {
    ajaxRadio(
      List(AnimalType.Dog, AnimalType.Cat),
      petType,
      (petSelected: AnimalType.Value) => {
        petType = Full(petSelected)

        (
          renderer.setHtml &
          orderSummaryRenderer.map(_.setHtml).openOr(Noop)
        )
      }
    ).toForm
  }

  def productDropdown = {
    val products = petType.map { animal =>
      Product.findAll(By(Product.animalType, animal))
    }.openOr(Nil)

    SHtml.ajaxSelectObj(
      products.map(product => (product, product.getNameAndSize)),
      chosenProduct,
      (possibleProduct: Product) => {
        chosenProduct = Full(possibleProduct)
        orderSummaryRenderer.map(_.setHtml).openOr(Noop)
      }
    )
  }

  def calculateTax(possibleState: String, possibleZip: String) = {
    state = possibleState
    zip = possibleZip

    if ((zip.length() > 4) && (state.toLowerCase() == "ga")) {
      val taxInfo = TaxJarService.findTaxAmoutAndRate(
        city,
        state,
        zip,
        12.99
      )

      taxDue = taxInfo._1
      taxRate = taxInfo._2
    } else {
      taxDue = 0D
      taxRate = 0D
    }

    priceAdditionsRenderer.map(_.setHtml).openOr(Noop)
  }

  def render = {
    val orderSummary = "#order-details" #> SHtml.idMemoize { renderer =>
      orderSummaryRenderer = Full(renderer)

      "#type span *" #> petType.map(_.toString) &
      "#size span *" #> chosenProduct.map(_.size.toString + " pounds") &
      "#product span *" #> chosenProduct.map(_.name.toString)
    }

    val orderTotal = {
      "#order" #> SHtml.idMemoize { renderer =>
        priceAdditionsRenderer = Full(renderer)

        val total = 12.99 + taxDue

        "#price-additions" #> ClearNodesIf((taxDue == 0D) && (coupon.isEmpty)) &
        "#price-additions" #> {
          "#tax" #> ClearNodesIf(taxDue == 0D) &
          "#promo-discount" #> ClearNodesIf(coupon.isEmpty) &
          "#promo-discount-note" #> ClearNodesIf(coupon.isEmpty) &
          "#tax #tax-amount" #> f"$taxDue%2.2f"
        } &
        {
          if(coupon.isEmpty) {
            "#order-total h3 [class!]" #> "promo" &
            "#order-total .monthly-charge [class!]" #> "promo" &
            "#order-total .monthly-charge .amount *" #> f"$$$total%2.2f"
          } else {
            "#order-total h3 [class+]" #> "promo" &
            "#order-total .monthly-charge [class+]" #> "promo" &
            "#order-total .monthly-charge *" #> {
              val freeMonths = coupon.map(_.freeMonths).openOr("0")
              if (freeMonths == 1) {
                s"FREE for first ${freeMonths} month"
              } else {
                s"FREE for first ${freeMonths} months"
              }
            }
          }
        }
      }
    }

    SHtml.makeFormsAjax andThen
    orderSummary &
    orderTotal &
    ".phone-portal [class+]" #> "current" &
    ".account-info" #> idMemoize { renderer =>
      ".pet-name" #> ajaxText(petName, petName = _) &
      ".pet-type-select" #> petTypeRadio(renderer) &
      ".product-container .product-select" #> productDropdown
    } &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> ajaxText(city, city = _) &
    "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#cardholder-name" #> text(cardholderName, cardholderName = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken)
  }
}
