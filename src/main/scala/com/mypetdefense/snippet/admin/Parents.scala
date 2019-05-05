package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import net.liftweb.util.ClearNodes
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper._

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
    mpdAdmin >>
    loggedIn
}

class Parents extends Loggable {
  var activeParents: List[User] = Nil
  var cancelledParents: List[CancelledUser] = Nil
  var parents: List[User] = Nil

  var parentsRenderer: Box[IdMemoizeTransform] = Empty
  
  var petType: Box[AnimalType.Value] = Empty
  var chosenProduct: Box[FleaTick] = Empty
  var petName = ""
  var petBreed = ""
  var petBirthday = ""
  var searchTerm = ""
  var email = ""

  val coupons = Coupon.findAll()
  val dogProducts = FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog))
  val catProducts = FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog))

  val stripeBaseUrl = Props.get("stripe.base.url") openOr "https://dashboard.stripe.com/test"
  val stripeInvoiceBaseURL = s"${stripeBaseUrl}/invoices"
  val stripeSubscriptionBaseURL = s"${stripeBaseUrl}/subscriptions"


  val nextShipDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  val birthdayFormat = new SimpleDateFormat("MM/dd/yyyy")
  val dateFormat = new SimpleDateFormat("MMM dd, yyyy")

  var parentDetailsRenderer: Box[IdMemoizeTransform] = Empty
  var currentParent: Box[User] = Empty

  def searchParents = {
    if (searchTerm == "")
      Alert("Search cannot be empty right now.")
    else {
      val formattedSearchTerm = searchTerm.split(" ").map(_ + ":*").mkString(" | ")

      val searchQuery = s"to_tsvector('english', coalesce(firstname,'') || ' ' || coalesce(lastname,'') || ' ' || coalesce(email,'')) @@ to_tsquery('english', '${formattedSearchTerm}')"

      val activeParentsSearch = User.findAll(
        By(User.userType, UserType.Parent),
        BySql(searchQuery,
          IHaveValidatedThisSQL("mike","2019-03-09")
        )
      )

      val cancelledParentsSearch = CancelledUser.findAll(
        BySql(searchQuery,
          IHaveValidatedThisSQL("mike","2019-03-09")
        )
      )

      activeParents = activeParentsSearch
      cancelledParents = cancelledParentsSearch

      val cancelledUsers = cancelledParents.map(getOldUserWithInfo).flatten
      parents = activeParents ++ cancelledUsers

      currentParent = Empty
      parentsRenderer.map(_.setHtml).openOr(Noop)
    }
  }

  def getCancelledUser(parent: User) = {
    CancelledUser.find(By(CancelledUser.user, parent.userId.get))
  }

  def getOldUserWithInfo(cancelledParent: CancelledUser) = {
    val cancelledUser = User.find(By(User.userId, cancelledParent.user.get))
    cancelledUser.map { user =>
      user
        .firstName(cancelledParent.firstName.get)
        .lastName(cancelledParent.lastName.get)
        .email(cancelledParent.email.get)
    }
  }

  def isCancelled_?(parent: Box[User]): Boolean = {
    parent.map(isCancelled_?).openOr(false)
  }

  def isCancelled_?(parent: User): Boolean = {
    parent.status.get == Status.Cancelled
  }

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
      if (animal == AnimalType.Dog)
        dogProducts
      else
        catProducts
    }.openOr(Nil)

    SHtml.ajaxSelectObj(
      products.map(product => (product, product.getNameAndSize)),
      chosenProduct,
      (possibleProduct: FleaTick) => chosenProduct = Full(possibleProduct)
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

        searchParents
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def deleteShipment(detailsRenderer: IdMemoizeTransform, shipment: Shipment)() = {
    shipment.shipmentLineItems.map(_.delete_!)
    shipment.delete_!

    detailsRenderer.setHtml
  }

  def refundShipment(detailsRenderer: IdMemoizeTransform, shipment: Shipment, parent: Box[User])() = {
    ParentService.refundShipment(shipment) match {
      case Full(refund) =>
        detailsRenderer.setHtml

      case _ =>
        Alert("Could not refund shipment. Please try again.")
    }
  }

  def displayNextShipDate(shipmentDate: Option[String], cancelled: Boolean) = {
    if (cancelled)
      "-"
    else
      shipmentDate.getOrElse("")
  }

  def parentInformationBinding(detailsRenderer: IdMemoizeTransform, subscription: Option[Subscription]) = {
    val parent = currentParent
    val address = parent.flatMap(_.addresses.toList.headOption)
    val agent = currentParent.map { user =>
      tryo(user.salesAgentId.get).openOr("")
    }

    def updateBillingStatus(status: Status.Value, oldSubscription: Subscription) = {
      oldSubscription.status(status).saveMe

      detailsRenderer.setHtml
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

    def changeEmail(parent: Box[User], email: String) = {
      val updatedParent = parent.map(_.refresh).flatten
      updatedParent.map(_.email(email).saveMe)

      Alert("Email Updated")
    }

    def updateParentName(name: String, namePart: String, parent: Box[User]) = {
      namePart match {
        case "firstName" => parent.map(_.firstName(name).saveMe)
        case "lastName" => parent.map(_.lastName(name).saveMe)
        case _ => Full(parent)
      }
    
      Noop
    }

    { 
      if (isCancelled_?(parent)) {
        ".parent-information" #> ClearNodes &
        ".parent-information [class-]" #> "active"
      } else {
        ".parent-information" #> PassThru
      }
    } &
    ".parent-information .address" #> {
      val firstName = parent.map(_.firstName.get).openOr("")
      val lastName = parent.map(_.lastName.get).openOr("")

      ".first-name" #> SHtml.ajaxText(firstName, possibleName => updateParentName(possibleName, "firstName", parent)) &
      ".last-name" #> SHtml.ajaxText(lastName, possibleName => updateParentName(possibleName, "lastName", parent)) &
      ".address-1" #> SHtml.ajaxText(street1, possibleAddress => updateAddress(possibleAddress, "street1", address)) &
      ".address-2" #> SHtml.ajaxText(street2, possibleAddress => updateAddress(possibleAddress, "street2", address)) &
      ".city" #> SHtml.ajaxText(city, possibleAddress => updateAddress(possibleAddress, "city", address)) &
      ".state" #> SHtml.ajaxText(state, possibleAddress => updateAddress(possibleAddress, "state", address)) &
      ".zip" #> SHtml.ajaxText(zip, possibleAddress => updateAddress(possibleAddress, "zip", address))
    } &
    ".parent-information .change-email" #> {
      val currentEmail = parent.map(_.email.get).openOr("")

      ".parent-email" #> SHtml.ajaxText(currentEmail, email = _) &
      ".update-email [onclick]" #> SHtml.ajaxInvoke(() => changeEmail(parent, email))
    } &
    ".parent-information .agent-name-container .agent *" #> agent
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
    
    def updateGrowthDelay(possibleDelay: String, pet: Pet) = {
      val sanitizedDelay = possibleDelay.replaceAll("[ months]","")
      val updatedDelay = tryo(sanitizedDelay.toInt).openOr(-1)

      if (updatedDelay != -1) {
        pet.nextGrowthDelay(updatedDelay).saveMe
      }

      Noop
    }

    def updatePetInfo(newInfo: String, infoType: String, pet: Pet, product: Box[FleaTick] = Empty) = {

      infoType match {
        case "name" => pet.name(newInfo).saveMe
        case "breed" => pet.breed(newInfo).saveMe
        case "birthday" =>
          val possibleBirthday = ParentService.parseWhelpDate(newInfo)
          pet.birthday(possibleBirthday.openOr(null)).saveMe
        case "product" => product.map { prod =>
          pet.fleaTick(prod).size(prod.size.get).saveMe
        }
        case _ => pet
      }
      
      Noop
    }

    def changePetProduct(
                          petType: AnimalType.Value,
                          currentProduct: Box[FleaTick],
                          pet: Pet
    ) = {
      val products = FleaTick.findAll(By(FleaTick.animalType, petType))

      SHtml.ajaxSelectObj(
        products.map(product => (product, product.getNameAndSize)),
        currentProduct,
        (possibleProduct: FleaTick) =>
          updatePetInfo("", "product", pet, Full(possibleProduct))
      )
    }

    ".parent-pets" #> ClearNodesIf(isCancelled_?(parent)) &
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
          val birthday = tryo(birthdayFormat.format(pet.birthday.get)).map(_.toString).openOr("")
          var nextGrowthDelay = tryo(pet.nextGrowthDelay.get.toString).openOr("")

          ".pet-name" #> SHtml.ajaxText(pet.name.get, possiblePetName => updatePetInfo(possiblePetName, "name", pet)) &
          ".pet-breed" #> SHtml.ajaxText(pet.breed.get, possibleBreed => updatePetInfo(possibleBreed, "breed", pet)) &
          ".pet-birthday *" #> SHtml.ajaxText(birthday, possibleBirthday => updatePetInfo(possibleBirthday, "birthday", pet)) &
          ".pet-type *" #> pet.animalType.toString &
          ".pet-product *" #> changePetProduct(pet.animalType.get, pet.fleaTick.obj, pet) &
          ".pet-delay-growth input" #> ajaxText(s"$nextGrowthDelay months", possibleDelay => updateGrowthDelay(possibleDelay, pet)) & 
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

    { 
      if (isCancelled_?(parent)) {
        ".parent-shipments [class+]" #> "active" &
        ".change-ship-date-container" #> ClearNodes &
        ".agent-info" #> PassThru
      } else {
        ".agent-info" #> ClearNodes
      }
    } andThen
    ".next-ship-date" #> ajaxText(updateNextShipDate, updateNextShipDate = _) &
    ".agent-name *" #> currentParent.map { user =>
      tryo(user.salesAgentId.get).openOr("")
    } &
    ".change-date [onClick]" #> SHtml.ajaxInvoke(() => updateShipDate) &
    ".shipment" #> shipments.sortWith(_.dateProcessed.get.getTime > _.dateProcessed.get.getTime).map { shipment =>
      
      val itemsShipped = shipment.shipmentLineItems.toList.map(_.getProductPetNameItemSize)

      ".paid-date *" #> tryo(dateFormat.format(shipment.dateProcessed.get)).openOr("-") &
      ".ship-date *" #> tryo(dateFormat.format(shipment.dateShipped.get)).openOr("-") &
      ".refund-date *" #> tryo(dateFormat.format(shipment.dateRefunded.get)).openOr("-") &
      ".amount-paid .stripe-invoice *" #> s"$$${shipment.amountPaid.get}" &
      ".amount-paid .stripe-invoice [href]" #> s"${stripeInvoiceBaseURL}/${shipment.stripePaymentId.get}" &
      ".pets ul" #> { itemsShipped.sortWith(_ < _).map { itemShipped =>
        ".pet-product *" #> itemShipped
      }} &
      ".address *" #> shipment.address.get &
      ".tracking-number-container .tracking-number [href]" #> s"https://tools.usps.com/go/TrackConfirmAction?tLabels=${shipment.trackingNumber.get}" &
      ".tracking-number-container .tracking-number *" #> shipment.trackingNumber.get &
      ".shipment-status *" #> shipment.shipmentStatus.toString &
      ".shipment-actions .delete [onclick]" #> Confirm(
        "Delete this shipment? This cannot be undone!",
        ajaxInvoke(deleteShipment(detailsRenderer, shipment) _)
      ) &
      ".shipment-actions .refund [onclick]" #> Confirm(
        "Refund this shipment? This cannot be undone!",
        ajaxInvoke(refundShipment(detailsRenderer, shipment, parent) _)
      ) &
      ".shipment-actions .refund" #> ClearNodesIf(
        (tryo(shipment.dateRefunded.get) != Full(null)) ||
        (tryo(shipment.stripeChargeId.get) == Full(null))
      )
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".parents [class+]" #> "current" &
    ".search-container" #> {
      ".search-term" #> ajaxText(searchTerm, searchTerm = _) &
      ".search-parents [onclick]" #> ajaxInvoke(searchParents _)
    } &
    ".parents-container" #> SHtml.idMemoize { renderer =>
      parentsRenderer = Full(renderer)

      "tbody" #> parents.sortWith(_.name < _.name).map { parent =>
        val refererName = parent.referer.obj.map(_.name.get)
        
        idMemoize { detailsRenderer =>
          val subscription = parent.getSubscription
          val nextShipDate = subscription.map(_.nextShipDate.get)

          ".parent" #> {
            ".name *" #> parent.name &
            ".email *" #> parent.email.get &
            ".billing-status *" #> subscription.map(_.status.get.toString.split("(?=\\p{Upper})").mkString(" ")) &
            ".referer *" #> refererName &
            ".ship-date *" #> tryo(displayNextShipDate(nextShipDate.map(dateFormat.format(_)), isCancelled_?(parent))).openOr("-") &
            ".actions .delete" #> ClearNodesIf(parent.activePets.size > 0) &
            ".actions .delete" #> ClearNodesIf(isCancelled_?(parent)) &
            ".actions .cancel" #> ClearNodesIf(isCancelled_?(parent)) &
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
              petBindings andThen
              shipmentBindings(detailsRenderer, subscription) andThen
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
}
