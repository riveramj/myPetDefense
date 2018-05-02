package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import net.liftweb.util.ClearNodes
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
import com.mypetdefense.actor._

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
  var petBreed = ""
  var petBirthday = ""

  val coupons = Coupon.findAll()

  val stripeBaseUrl = Props.get("stripe.base.url") openOr "https://dashboard.stripe.com/test"
  val stripePaymentsBaseURL = s"${stripeBaseUrl}/invoices"
  val stripeSubscriptionBaseURL = s"${stripeBaseUrl}/subscriptions"


  val nextShipDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  val birthdayFormat = new SimpleDateFormat("MM/dd/yyyy")
  val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  var parentDetailsRenderer: Box[IdMemoizeTransform] = Empty
  var currentParent: Box[User] = Empty

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

  def addCoupon(parent: Box[User], updatedCoupon: Box[Coupon]) = {
    parent.map(_.coupon(updatedCoupon).saveMe)

    ParentService.updateCoupon(parent.map(_.stripeId.get).openOr(""), updatedCoupon.map(_.couponCode.get))

    S.redirectTo(Parents.menu.loc.calcDefaultHref)
  }

  def addPet(possibleParent: Box[User], renderer: IdMemoizeTransform) = {
    val validateFields = List(
      checkEmpty(petName, ".new-pet-name")
    ).flatten

    if(validateFields.isEmpty) {
      (for {
        pet <- petType
        product <- chosenProduct
        parent <- possibleParent
        size = product.size.get
      } yield {
        ParentService.addNewPet(
          oldUser = parent,
          name = petName,
          animalType = pet,
          size = size,
          product = product,
          breed = petBreed,
          birthday = petBirthday
        )
      }).flatMap(identity) match {
        case Full(pet) =>
          petName = ""
          petBreed = ""
          petBirthday = ""
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

  def deletePet(parent: Box[User], pet: Pet, renderer: IdMemoizeTransform)() = {
    ParentService.removePet(parent, pet) match {
      case Full(_) =>
        renderer.setHtml
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def deleteParent(parent: User)() = {
    ParentService.removeParent(parent, true) match {
      case Full(_) =>
        S.redirectTo(Parents.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def cancelParent(parent: User)() = {
    val pets: List[Pet] = parent.pets.toList

    pets.map(ParentService.removePet(parent, _))

    ParentService.removeParent(parent) match {
      case Full(_) =>
        EmailActor ! ParentCancelledAccountEmail(parent)

        S.redirectTo(Parents.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def deleteShipment(detailsRenderer: IdMemoizeTransform, shipment: Shipment)() = {
    shipment.shipmentLineItems.map(_.delete_!)
    shipment.delete_!

    detailsRenderer.setHtml
  }

  def parentInformationBinding(detailsRenderer: IdMemoizeTransform, subscription: Option[Subscription]) = {
    val parent = currentParent
    val address = parent.flatMap(_.addresses.toList.headOption)

    var stripeSubscriptionId = subscription.map(_.stripeSubscriptionId.get).getOrElse("")

    def updateBillingStatus(status: Status.Value, oldSubscription: Subscription) = {
      oldSubscription.status(status).saveMe

      detailsRenderer.setHtml
    }

    def updateStripeSubscriptionId(oldSubscription: Option[Subscription]) = {
      oldSubscription.map(_.stripeSubscriptionId(stripeSubscriptionId).saveMe)

      {
        Alert("Id has been updated") &
        detailsRenderer.setHtml
      }
    }

    val street1 = address.map(_.street1.get).openOr("")
    val street2 = address.map(_.street2.get).openOr("")
    val city = address.map(_.city.get).openOr("")
    val state = address.map(_.state.get).openOr("")
    val zip = address.map(_.zip.get).openOr("")

    def updateAddress(updatedAddressPart: String, addressPart: String, address: Box[Address]) = {
      
      addressPart match {
        case "street1" => 
          address.map(_.street1(updatedAddressPart).saveMe)
        case "street2" => 
          address.map(_.street2(updatedAddressPart).saveMe)
        case "city" => 
          address.map(_.city(updatedAddressPart).saveMe)
        case "state" => 
          address.map(_.state(updatedAddressPart).saveMe)
        case "zip" => 
          address.map(_.zip(updatedAddressPart).saveMe)
        case _ =>
          address
      }

      Noop
    }

    ".parent-information .address" #> {
      ".address-1" #> SHtml.ajaxText(street1, possibleAddress => updateAddress(possibleAddress, "street1", address)) &
      ".address-2" #> SHtml.ajaxText(street2, possibleAddress => updateAddress(possibleAddress, "street2", address)) &
      ".city" #> SHtml.ajaxText(city, possibleAddress => updateAddress(possibleAddress, "city", address)) &
      ".state" #> SHtml.ajaxText(state, possibleAddress => updateAddress(possibleAddress, "state", address)) &
      ".zip" #> SHtml.ajaxText(zip, possibleAddress => updateAddress(possibleAddress, "zip", address))
    } &
    ".parent-information .billing-status" #> subscription.map { oldSubscription =>
      val oldStatus = oldSubscription.status.get

      oldStatus match {
        case Status.BillingSuspended =>
          "#parent-billing-status .stripe-subscription [class+]" #> "past-due" &
          "#parent-billing-status .stripe-subscription [href]" #> s"${stripeSubscriptionBaseURL}/${oldSubscription.stripeSubscriptionId.get}" &
          "#parent-billing-status .stripe-subscription *" #> "Suspended due to billing" &
          ".change-to-active [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.Active, oldSubscription)) &
          ".change-to-user-suspended [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.UserSuspended, oldSubscription)) &
          ".change-to-billing-suspended" #> ClearNodes

        case Status.UserSuspended =>
          "#parent-billing-status .stripe-subscription *" #> "User Suspended" &
          "#parent-billing-status .stripe-subscription [href]" #> s"${stripeSubscriptionBaseURL}/${oldSubscription.stripeSubscriptionId.get}" &
          ".change-to-active [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.Active, oldSubscription)) &
          ".change-to-billing-suspended [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.BillingSuspended, oldSubscription))&
          ".change-to-user-suspended" #> ClearNodes

        case Status.Active =>
          "#parent-billing-status .stripe-subscription *" #> "Active" &
          "#parent-billing-status .stripe-subscription [href]" #> s"${stripeSubscriptionBaseURL}/${oldSubscription.stripeSubscriptionId.get}" &
          ".change-to-billing-suspended [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.BillingSuspended, oldSubscription)) &
          ".change-to-user-suspended [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.UserSuspended, oldSubscription)) &
          ".change-to-active" #> ClearNodes

        case _ =>
          "#parent-billing-status .stripe-subscription *" #> oldStatus.toString &
          "#parent-billing-status .stripe-subscription [href]" #> s"${stripeSubscriptionBaseURL}/${oldSubscription.stripeSubscriptionId.get}" &
          ".change-billing-status [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.Active, oldSubscription)) &
          ".change-to-user-suspended [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.UserSuspended, oldSubscription)) &
          ".change-to-billing-suspended [onClick]" #> SHtml.ajaxInvoke(() => updateBillingStatus(Status.BillingSuspended, oldSubscription))
      }
    } &
    ".parent-information .change-stripe-subscription .stripe-subscription-id" #> ajaxText(stripeSubscriptionId, stripeSubscriptionId = _) &
    ".parent-information .change-stripe-subscription .change-id [onClick]" #> Confirm(
      s"Update Stripe Subscription Id? This can really mess things up!",
      ajaxInvoke(() => updateStripeSubscriptionId(subscription))
    )
  }

  def petBindings = {
    val parent = currentParent
    var chosenCoupon = parent.flatMap(_.coupon.obj)

    def couponDropdown = {
      SHtml.ajaxSelectObj(
        (Empty, "No Coupon") +: coupons.map(coupon => (Full(coupon), coupon.couponCode.get)),
        Full(chosenCoupon),
        (possibleCoupon: Box[Coupon]) => chosenCoupon = possibleCoupon
      )
    }

    ".parent-pets" #> idMemoize { renderer =>
      ".create" #> {
        ".new-pet-name" #> ajaxText(petName, petName = _) &
        ".new-pet-breed" #> ajaxText(petBreed, petBreed = _) &
        ".new-pet-birthday" #> ajaxText(petBirthday, petBirthday = _) &
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
          val birthday = tryo(birthdayFormat.format(pet.birthday.get))

          ".pet-name *" #> pet.name &
          ".pet-breed *" #> pet.breed &
          ".pet-birthday *" #> birthday &
          ".pet-type *" #> pet.animalType.toString &
          ".pet-product *" #> pet.product.obj.map(_.getNameAndSize) &
          ".actions .delete [onclick]" #> Confirm(s"Delete ${pet.name}?",
            ajaxInvoke(deletePet(parent, pet, renderer) _)
          )
        }
      }
    }
  }

  def shipmentBindings(detailsRenderer: IdMemoizeTransform, subscription: Option[Subscription]) = {
    val parent = currentParent
    val nextShipDate = subscription.map(_.nextShipDate.get)

    val shipments: List[Shipment] = subscription.map { sub => 
      Shipment.findAll(By(Shipment.subscription, sub))
    }.getOrElse(Nil)

    var updateNextShipDate = nextShipDate.map(date => nextShipDateFormat.format(date).toString).getOrElse("")

    def updateShipDate() = {
      val updatedDate = nextShipDateFormat.parse(updateNextShipDate)

      subscription.map { oldSubscription =>
        ParentService.updateNextShipBillDate(oldSubscription, parent, updatedDate)
        oldSubscription.nextShipDate(updatedDate).saveMe
      }

      detailsRenderer.setHtml
    }

    ".next-ship-date" #> ajaxText(updateNextShipDate, updateNextShipDate = _) &
    ".change-date [onClick]" #> SHtml.ajaxInvoke(() => updateShipDate) &
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
      ".tracking-number *" #> shipment.trackingNumber.get &
      ".shipment-actions .delete [onclick]" #> Confirm(
        "Delete this shipment? This cannot be undone!",
        ajaxInvoke(deleteShipment(detailsRenderer, shipment) _)
      )
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".parents [class+]" #> "current" &
    "#active-parents-export [href]" #> Parents.activeParentsCsvMenu.loc.calcDefaultHref &
    "tbody" #> parents.sortWith(_.name < _.name).map { parent =>
      val refererName = parent.referer.obj.map(_.name.get)
      idMemoize { detailsRenderer =>
        val subscription = parent.getSubscription
        val nextShipDate = subscription.map(_.nextShipDate.get)

        ".parent" #> {
          ".name *" #> parent.name &
          ".email *" #> parent.email &
          ".billing-status *" #> subscription.map(_.status.get.toString.split("(?=\\p{Upper})").mkString(" ")) &
          ".referer *" #> refererName &
          ".ship-date *" #> nextShipDate.map(dateFormat.format(_)) &
          ".actions .delete" #> ClearNodesIf(parent.activePets.size > 0) &
          ".actions .delete [onclick]" #> Confirm(
            s"Delete ${parent.name}? This will remove all billing info subscriptions. Cannot be undone!",
            ajaxInvoke(deleteParent(parent) _)
          ) &
          ".actions .cancel [onclick]" #> Confirm(
            s"Cancel ${parent.name}? This will cancel the user's account.",
            ajaxInvoke(cancelParent(parent) _)
          ) &
          "^ [onclick]" #> ajaxInvoke(() => {
            if (currentParent.isEmpty) {
              currentParent = Full(parent)
            } else {
              currentParent = Empty
            }

            detailsRenderer.setHtml
          })
        } & 
        ".info [class+]" #> {if (currentParent.isEmpty) "" else "expanded"} &
        "^ [class+]" #> {if (currentParent.isEmpty) "" else "expanded"} &
        ".parent-info" #> {
          if (!currentParent.isEmpty) {
            petBindings &
            shipmentBindings(detailsRenderer, subscription) &
            parentInformationBinding(detailsRenderer, subscription)
          }
          else {
            "^" #> ClearNodes
          }
        }
      }
    }
  }
}
