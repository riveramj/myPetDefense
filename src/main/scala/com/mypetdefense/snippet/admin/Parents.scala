package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.ClearNodesIf

object Parents extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Parents") / "admin" / "parents" >>
    adminUser >>
    loggedIn

  val activeParentsCsvMenu = Menu.i("Active Parents") / "admin" / "parents" / "export_active_parents.csv" >>
    adminUser >>
    loggedIn >>
    EarlyResponse(exportActiveParents _)

  def exportActiveParents: Box[LiftResponse] = {
      val csvHeaders = "Name" :: "Email" :: "Address" :: "Pet Name" :: "Animal Type" :: "Pet Size" :: Nil

      val csvRows: List[List[String]] = {
        val pets = Pet.findAll(By(Pet.status, Status.Active))
        
        {
          for {
            pet <- pets
            user <- pet.user.obj
            address <- user.addresses.toList.headOption
          } yield {

            user.name ::
            user.email.get ::
            s"${address.street1.get} ${address.street2.get} ${address.city.get} ${address.state.get} ${address.zip.get}" ::
            pet.name.get ::
            pet.animalType.get.toString ::
            pet.size.get.toString  ::
            Nil
          }
        }
      }

      val resultingCsv = (List(csvHeaders) ++ csvRows).map(_.mkString(",")).mkString("\n")

        Some(new InMemoryResponse(
          resultingCsv.getBytes("UTF-8"),
          List(
            "Content-Type" -> "binary/octet-stream",
            "Content-Disposition" -> "attachment; filename=\"data.csv\""
            ),
          Nil,
          200
        ))
    }
}

class Parents extends Loggable {
  val parents = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))
  
  var petType: Box[AnimalType.Value] = Empty
  var chosenProduct: Box[Product] = Empty
  var petName = ""

  val coupons = Coupon.findAll()

  val stripePaymentsBaseURL = Props.get("stripe.payments.url") openOr "https://dashboard.stripe.com/test/"

  val nextShipDateFormat= new SimpleDateFormat("MM/dd/yyyy")

  def petTypeRadio(renderer: IdMemoizeTransform) = {
    ajaxRadio(
      List(AnimalType.Dog, AnimalType.Cat),
      petType,
      (petSelected: AnimalType.Value) => {
        petType = Full(petSelected)
        renderer.setHtml
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
      (possibleProduct: Product) => chosenProduct = Full(possibleProduct)
    )
  }

  def addCoupon(parent: User, updatedCoupon: Box[Coupon]) = {
    parent.coupon(updatedCoupon).saveMe

    ParentService.updateCoupon(parent.stripeId.get, updatedCoupon.map(_.couponCode.get))

    S.redirectTo(Parents.menu.loc.calcDefaultHref)
  }

  def addPet(parent: User, renderer: IdMemoizeTransform) = {
    val validateFields = List(
      checkEmpty(petName, ".new-pet-name")
    ).flatten

    if(validateFields.isEmpty) {
      (for {
        pet <- petType
        product <- chosenProduct
        size = product.size.get
      } yield {
        ParentService.addNewPet(
          user = parent,
          name = petName,
          animalType = pet,
          size = size,
          product = product
        )
      }).flatMap(identity) match {
        case Full(pet) =>
          petName = ""
          petType = Empty
          chosenProduct = Empty

          renderer.setHtml
        case other =>
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deletePet(parent: User, pet: Pet, renderer: IdMemoizeTransform)() = {
    ParentService.removePet(parent, pet) match {
      case Full(_) =>
        renderer.setHtml
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def deleteParent(parent: User)() = {
    ParentService.removeParent(parent) match {
      case Full(_) =>
        S.redirectTo(Parents.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".parents [class+]" #> "current" &
    "#active-parents-export [href]" #> Parents.activeParentsCsvMenu.loc.calcDefaultHref &
    "tbody" #> parents.sortWith(_.name < _.name).map { parent =>
      val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

      val subscription = Subscription.find(By(Subscription.user, parent))
      val nextShipDate = subscription.map(_.nextShipDate.get)

      var chosenCoupon: Box[Coupon] = parent.coupon.obj

      def couponDropdown = {
        SHtml.ajaxSelectObj(
          (Empty, "No Coupon") +: coupons.map(coupon => (Full(coupon), coupon.couponCode.get)),
          Full(chosenCoupon),
          (possibleCoupon: Box[Coupon]) => chosenCoupon = possibleCoupon
        )
      }

      def petBindings = {
        ".parent-pets" #> idMemoize { renderer =>
          ".create" #> {
            ".new-pet-name" #> ajaxText(petName, petName = _) &
            ".pet-type-select" #> petTypeRadio(renderer) &
            ".product-container .product-select" #> productDropdown &
            ".create-item-container .create-item" #> SHtml.ajaxSubmit("Add Pet", () => addPet(parent, renderer))
          } & 
          ".add-coupon" #> {
            ".coupon-container .coupon-select" #> couponDropdown &
            ".create-coupon-container .create-coupon" #> SHtml.ajaxSubmit("Change Coupon", () => addCoupon(parent, chosenCoupon))
          } & 
          {
            val pets = Pet.findAll(
              By(Pet.user, parent),
              By(Pet.status, Status.Active)
            )

            ".pet" #> pets.map { pet =>
              ".pet-name *" #> pet.name &
              ".pet-type *" #> pet.animalType.toString &
              ".pet-product *" #> pet.product.obj.map(_.getNameAndSize) &
              ".actions .delete [onclick]" #> Confirm(s"Delete ${pet.name}?",
                ajaxInvoke(deletePet(parent, pet, renderer))
                )
            }
          }
        }
      }

      def shipmentBindings = {
        val shipments: List[Shipment] = subscription.map { sub => 
          Shipment.findAll(By(Shipment.subscription, sub))
        }.openOr(Nil)

      ".shipment" #> shipments.sortWith(_.dateProcessed.get.getTime > _.dateProcessed.get.getTime).map { shipment =>
          val itemsShipped = shipment.shipmentLineItems.toList.map(_.getShipmentItem)

          ".paid-date *" #> tryo(dateFormat.format(shipment.dateProcessed.get)).openOr("-") &
          ".ship-date *" #> tryo(dateFormat.format(shipment.dateShipped.get)).openOr("-") &
          ".amount-paid .stripe-payment *" #> s"$$${shipment.amountPaid.get}" &
          ".amount-paid .stripe-payment [href]" #> s"${stripePaymentsBaseURL}/${shipment.stripePaymentId.get}" &
          ".pets ul" #> { itemsShipped.sortWith(_ < _).map { itemShipped =>
            ".pet-product *" #> itemShipped
          }} &
          ".address *" #> shipment.address.get &
          ".tracking-number *" #> shipment.trackingNumber.get
        }
      }

      def parentInformationBinding = {
        val address = parent.addresses.toList.headOption
        var updateNextShipDate = nextShipDate.map(date => nextShipDateFormat.format(date).toString).getOrElse("")

        def updateShipDate() = {
          val updatedDate = nextShipDateFormat.parse(updateNextShipDate)

          subscription.map { oldSubscription =>
            ParentService.updateNextShipBillDate(oldSubscription, Full(parent), updatedDate)
            oldSubscription.nextShipDate(updatedDate).saveMe
          }
          
          S.redirectTo(Parents.menu.loc.calcDefaultHref)
        }

        ".address" #> {
          ".address-1 *" #> address.map(_.street1.get) &
          ".address-2 *" #> address.map(_.street2.get) &
          ".city *" #> address.map(_.city.get) &
          ".state *" #> address.map(_.state.get) &
          ".zip *" #> address.map(_.zip.get) 
        } &
        ".next-ship-date" #> ajaxText(updateNextShipDate, updateNextShipDate = _) &
        ".change-date [onClick]" #> SHtml.ajaxInvoke(() => updateShipDate)
      }

      ".parent" #> {
        ".name *" #> parent.name &
        ".email *" #> parent.email &
        ".phone *" #> parent.phone &
        ".coupon *" #> parent.coupon.obj.map(_.couponCode.get) &
        ".referer *" #> parent.referer.obj.map(_.name.get) &
        ".ship-date *" #> nextShipDate.map(dateFormat.format(_)) &
        ".actions .delete" #> ClearNodesIf(parent.activePets.size > 0) &
        ".actions .delete [onclick]" #> Confirm(s"Delete ${parent.name}? This will remove all billing info subscriptions. Cannot be undone!",
          ajaxInvoke(deleteParent(parent))
        ) 
      } &
      petBindings &
      shipmentBindings &
      parentInformationBinding
    }
  }
}
