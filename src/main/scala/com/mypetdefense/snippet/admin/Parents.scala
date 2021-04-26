package com.mypetdefense.snippet
package admin

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.SupportAction.{SupportAddedPet, SupportCanceledAccount, SupportRemovedPet}
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.{ClearNodesIf, SecurityContext, SplitTitleCase}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import java.text.SimpleDateFormat
import java.time.{LocalDate, ZoneId}
import java.util.Date
import scala.xml.{Elem, NodeSeq}

object Parents extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Parents") / "admin" / "parents" >>
    mpdAdmin >>
    loggedIn
}

class Parents extends Loggable {
  var activeParents: List[User]             = Nil
  var cancelledParents: List[CancelledUser] = Nil
  var parents: List[User]                   = Nil
  var selectedPet: Box[Pet]                 = Empty

  var petProductsRender: Box[IdMemoizeTransform] = Empty
  var parentsRenderer: Box[IdMemoizeTransform]   = Empty
  var petsRenderer: Box[IdMemoizeTransform]      = Empty

  var petType: Box[AnimalType.Value] = Empty
  var chosenProduct: Box[FleaTick]   = Empty
  var petName                        = ""
  var petBreed                       = ""
  var petBirthday                    = ""
  var searchTerm                     = ""
  var email                          = ""

  val coupons: List[Coupon] = Coupon.findAll()
  val dogProducts: List[FleaTick] =
    FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog)).filter(_.isZoGuard_?)
  val catProducts: List[FleaTick] = FleaTick.zoGuardCat.toList

  val stripeBaseUrl: String =
    Props.get("stripe.base.url") openOr "https://dashboard.stripe.com/test"
  val stripeInvoiceBaseURL      = s"${stripeBaseUrl}/invoices"
  val stripeSubscriptionBaseURL = s"${stripeBaseUrl}/subscriptions"

  val nextShipDateFormat = new SimpleDateFormat("MM/dd/yyyy")
  val birthdayFormat     = new SimpleDateFormat("MM/dd/yyyy")
  val dateFormat         = new SimpleDateFormat("MMM dd, yyyy")
  val dateTimeFormat     = new SimpleDateFormat("MM/dd/yy h:mm a")

  var parentDetailsRenderer: Box[IdMemoizeTransform] = Empty
  var currentParent: Box[User]                       = Empty

  def searchParents: JsCmd = {
    searchTerm match {
      case empty if empty.isEmpty => Alert("Search cannot be empty right now.")
      case zipCode if tryo(zipCode.toInt).isDefined =>
        val addresses = Address.findAll(
          By(Address.zip, zipCode)
        )
        activeParents = addresses.flatMap(_.user.obj)
        cancelledParents = Nil

        parents = activeParents

        currentParent = Empty
        parentsRenderer.map(_.setHtml).openOr(Noop)

      case userInfo =>
        val formattedSearchTerm = userInfo.trim.split(" ").map(_ + ":*").mkString(" | ")

        val searchQuery =
          s"to_tsvector('english', coalesce(firstname,'') || ' ' || coalesce(lastname,'') || ' ' || coalesce(email,'')) @@ to_tsquery('english', '${formattedSearchTerm}')"

        val activeParentsSearch = User.findAll(
          By(User.userType, UserType.Parent),
          BySql(searchQuery, IHaveValidatedThisSQL("mike", "2019-03-09"))
        )

        val cancelledParentsSearch = CancelledUser.findAll(
          BySql(searchQuery, IHaveValidatedThisSQL("mike", "2019-03-09"))
        )

        activeParents = activeParentsSearch
        cancelledParents = cancelledParentsSearch

        val cancelledUsers = cancelledParents.flatMap(getOldUserWithInfo)
        parents = activeParents ++ cancelledUsers

        currentParent = Empty
        parentsRenderer.map(_.setHtml).openOr(Noop)
    }

  }

  def getCancelledUser(parent: User): Box[CancelledUser] = {
    CancelledUser.find(By(CancelledUser.user, parent.userId.get))
  }

  def getOldUserWithInfo(cancelledParent: CancelledUser): Box[User] = {
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

  def petTypeRadio(renderer: IdMemoizeTransform): NodeSeq = {
    ajaxRadio(
      List(AnimalType.Dog, AnimalType.Cat),
      petType,
      (petSelected: AnimalType.Value) => {
        petType = Full(petSelected)
        renderer.setHtml
      }
    ).toForm
  }

  def productDropdown: Elem = {
    val products = petType.map { animal =>
      if (animal == AnimalType.Dog)
        dogProducts
      else
        catProducts
    }.openOr(Nil)

    SHtml.ajaxSelectObj(
      (Empty, "Choose Size") +: products.map(product => (Full(product), product.getNameAndSize)),
      Full(chosenProduct),
      (possibleProduct: Box[FleaTick]) => chosenProduct = possibleProduct
    )
  }

  def addCoupon(parent: Box[User], updatedCoupon: Box[Coupon]): Nothing = {
    parent.map(_.coupon(updatedCoupon).saveMe)

    ParentService.updateCoupon(
      parent.map(_.stripeId.get).openOr(""),
      updatedCoupon.map(_.couponCode.get)
    )

    S.redirectTo(Parents.menu.loc.calcDefaultHref)
  }

  def addPet(possibleParent: Box[User], renderer: IdMemoizeTransform): JsCmd = {
    val validateFields = List(
      checkEmpty(petName, ".new-pet-name")
    ).flatten

    if (validateFields.isEmpty) {
      (for {
        pet     <- petType
        product <- chosenProduct
        parent  <- possibleParent
        size = product.size.get
      } yield {
        val actionReport = SupportAddedPet(
          possibleParent.map(_.userId.get).openOrThrowException("No Parent Id"),
          Some(SecurityContext.currentUserId),
          0L,
          petName
        )

        ParentService.addNewPet(
          oldUser = parent,
          name = petName,
          animalType = pet,
          size = size,
          product = product,
          isUpgraded = parent.subscription.obj.map(_.isUpgraded.get).openOr(false),
          breed = petBreed,
          birthday = petBirthday,
          actionLog = Right(actionReport)
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

  def deletePet(parent: Box[User], pet: Pet, renderer: IdMemoizeTransform)(): JsCmd = {
    val actionLog = SupportRemovedPet(
      parent.map(_.userId.get).openOr(0L),
      Some(SecurityContext.currentUserId),
      pet.petId.get,
      pet.name.get
    )

    ParentService.removePet(parent, pet, actionLog) match {
      case Full(_) =>
        renderer.setHtml
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def deleteParent(parent: User)(): Alert = {
    val actionLog = SupportCanceledAccount(
      parent.userId.get,
      Some(SecurityContext.currentUserId),
      parent.subscription.obj.map(_.subscriptionId.get).openOr(0L)
    )
    ParentService.removeParent(parent, actionLog, true) match {
      case Full(_) =>
        S.redirectTo(Parents.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def cancelParent(parent: User)(): JsCmd = {
    val pets: List[Pet] = parent.pets.toList

    pets.map { pet =>
      val actionLog = SupportRemovedPet(
        parent.userId.get,
        Some(SecurityContext.currentUserId),
        pet.petId.get,
        pet.name.get
      )

      ParentService.removePet(parent, pet, actionLog)
    }

    val actionLog = SupportCanceledAccount(
      parent.userId.get,
      Some(SecurityContext.currentUserId),
      parent.subscription.obj.map(_.subscriptionId.get).openOr(0L)
    )

    ParentService.removeParent(parent, actionLog = actionLog) match {
      case Full(_) =>
        EmailActor ! ParentCancelledAccountEmail(parent)

        searchParents
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def deleteShipment(detailsRenderer: IdMemoizeTransform, shipment: Shipment)(): JsCmd = {
    shipment.shipmentLineItems.map(_.delete_!)
    shipment.delete_!

    detailsRenderer.setHtml
  }

  def refundShipment(
      detailsRenderer: IdMemoizeTransform,
      shipment: Shipment,
      parent: Box[User]
  )(): JsCmd = {
    ParentService.refundShipment(shipment) match {
      case Full(refund) =>
        detailsRenderer.setHtml

      case _ =>
        Alert("Could not refund shipment. Please try again.")
    }
  }

  def displayNextShipDate(shipmentDate: Option[String], cancelled: Boolean): String = {
    if (cancelled)
      "-"
    else
      shipmentDate.getOrElse("")
  }

  def subscriptionBinding(
      detailsRenderer: IdMemoizeTransform,
      oldSubscription: Box[Subscription]
  ): CssBindFunc = {
    val subscription       = oldSubscription.map(_.reload)
    val subscriptionStatus = subscription.map(_.status.get)
    val subscriptionLevel = if (subscription.exists(_.isUpgraded.get))
      "Health & Wellness"
    else
      "Flea & Tick only"

    def setSubscriptionStatus(status: Status.Value) = {
      val updatedSubscription = subscription.map(_.status(status).saveMe())

      if (status == Status.Active) {
        val tomorrow = Date.from(
          LocalDate
            .now(ZoneId.of("America/New_York"))
            .atStartOfDay(ZoneId.of("America/New_York"))
            .plusDays(1)
            .toInstant
        )
        updatedSubscription.map { subscriptionToUpdate =>
          ParentService.updateNextShipBillDate(subscriptionToUpdate, tomorrow)
        }

        Alert("Subscription Resumed. Will ship tomorrow.") &
          detailsRenderer.setHtml()
      } else {
        val nextShipDate = new SimpleDateFormat("MM/dd/yyyy").parse("08/01/2022")
        updatedSubscription.map { subscriptionToUpdate =>
          ParentService.updateNextShipBillDate(subscriptionToUpdate, nextShipDate)
        }

        Alert("Subscription Paused.") &
          detailsRenderer.setHtml()
      }
    }

    def upgradeAccount = {
      SubscriptionService.upgradeAccount(subscription, true) &
      detailsRenderer.setHtml
    }

    ".parent-subscription" #> ClearNodesIf(isCancelled_?(currentParent)) &
      ".parent-subscription" #> {
        ".subscription-status *" #> subscriptionStatus.map(_.toString) &
          ".pause-subscription" #> ClearNodesIf(subscriptionStatus.contains(Status.Paused)) &
          ".pause-subscription [onclick]" #> SHtml.ajaxInvoke(() =>
            setSubscriptionStatus(Status.Paused)
          ) &
          ".resume-subscription" #> ClearNodesIf(subscriptionStatus.contains(Status.Active)) &
          ".resume-subscription [onclick]" #> SHtml.ajaxInvoke(() =>
            setSubscriptionStatus(Status.Active)
          ) &
          ".subscription-level *" #> subscriptionLevel &
          ".upgrade-subscription" #> ClearNodesIf(subscription.exists(_.isUpgraded.get)) &
          ".upgrade-subscription [onclick]" #> SHtml.ajaxInvoke(() => upgradeAccount)
      }
  }

  def parentInformationBinding(
      detailsRenderer: IdMemoizeTransform,
      subscription: Option[Subscription]
  ): CssBindFunc = {
    val parent  = currentParent
    val address = parent.flatMap(_.addresses.toList.headOption)
    val agent = currentParent.map { user =>
      if (user.salesAgentId.get == null || user.salesAgentId.get.isEmpty)
        "-"
      else
        user.salesAgentId.get
    }

    def updateBillingStatus(status: Status.Value, oldSubscription: Subscription) = {
      oldSubscription.status(status).saveMe

      detailsRenderer.setHtml
    }

    val street1 = address.map(_.street1.get).openOr("")
    val street2 = address.map(_.street2.get).openOr("")
    val city    = address.map(_.city.get).openOr("")
    val state   = address.map(_.state.get).openOr("")
    val zip     = address.map(_.zip.get).openOr("")

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
      val updatedParent = parent.map(_.reload)
      updatedParent.map(_.email(email).saveMe)

      Alert("Email Updated")
    }

    def updateParentName(name: String, namePart: String, parent: Box[User]) = {
      namePart match {
        case "firstName" => parent.map(_.firstName(name).saveMe)
        case "lastName"  => parent.map(_.lastName(name).saveMe)
        case _           => Full(parent)
      }

      Noop
    }

    def sendResetPasswordEmail(possibleUser: Box[User]) = {
      possibleUser match {
        case Full(user) =>
          val userWithResetKey = KeyService.createResetKey(user)
          EmailActor ! SendPasswordResetEmail(userWithResetKey)
          Alert("Sent user reset email.")
        case _ =>
          Alert("Couldn't send reset email.")
      }
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
        val lastName  = parent.map(_.lastName.get).openOr("")

        ".first-name" #> SHtml.ajaxText(
          firstName,
          possibleName => updateParentName(possibleName, "firstName", parent)
        ) &
          ".last-name" #> SHtml.ajaxText(
            lastName,
            possibleName => updateParentName(possibleName, "lastName", parent)
          ) &
          ".address-1" #> SHtml.ajaxText(
            street1,
            possibleAddress => updateAddress(possibleAddress, "street1", address)
          ) &
          ".address-2" #> SHtml.ajaxText(
            street2,
            possibleAddress => updateAddress(possibleAddress, "street2", address)
          ) &
          ".city" #> SHtml.ajaxText(
            city,
            possibleAddress => updateAddress(possibleAddress, "city", address)
          ) &
          ".state" #> SHtml.ajaxText(
            state,
            possibleAddress => updateAddress(possibleAddress, "state", address)
          ) &
          ".zip" #> SHtml.ajaxText(
            zip,
            possibleAddress => updateAddress(possibleAddress, "zip", address)
          )
      } &
      ".parent-information" #> {
        val currentEmail = parent.map(_.email.get).openOr("")

        ".reset-password [onclick]" #> SHtml.ajaxInvoke(() => sendResetPasswordEmail(parent)) &
          ".change-email" #> {
            ".parent-email" #> SHtml.ajaxText(currentEmail, email = _) &
              ".update-email [onclick]" #> SHtml.ajaxInvoke(() => changeEmail(parent, email))
          }
      } &
      ".parent-information .agent-name-container .agent *" #> agent
  }

  def showProducts(pet: Box[Pet])(): JsCmd = {
    selectedPet = pet

    petProductsRender.map(_.setHtml()).openOr(Noop)
  }

  def petBindings = {
    val parent       = currentParent
    var chosenCoupon = parent.flatMap(_.coupon.obj)

    def couponDropdown = {
      SHtml.ajaxSelectObj(
        (Empty, "No Coupon") +: coupons.map(coupon => (Full(coupon), coupon.couponCode.get)),
        Full(chosenCoupon),
        (possibleCoupon: Box[Coupon]) => chosenCoupon = possibleCoupon
      )
    }

    def updateGrowthDelay(possibleDelay: String, pet: Pet) = {
      val sanitizedDelay = possibleDelay.replaceAll("[ months]", "")
      val updatedDelay   = tryo(sanitizedDelay.toInt).openOr(-1)

      if (updatedDelay != -1) {
        pet.nextGrowthDelay(updatedDelay).saveMe
      }

      Noop
    }

    def updatePetInfo(
        newInfo: String,
        infoType: String,
        pet: Box[Pet],
        subscriptionBox: Box[SubscriptionBox] = Empty,
        product: Box[FleaTick] = Empty
    ) = {

      infoType match {
        case "name"  => pet.map(_.name(newInfo).saveMe)
        case "breed" => pet.map(_.breed(newInfo).saveMe)
        case "birthday" =>
          val possibleBirthday = ParentService.parseWhelpDate(newInfo)
          pet.map(_.birthday(possibleBirthday.openOr(null)).saveMe)
        case "product" => product.map { prod => subscriptionBox.map(_.fleaTick(prod).saveMe()) }
        case _         => pet
      }

      Noop
    }

    def savePet(
                 subscriptionBox: Box[SubscriptionBox],
                 updatedFleaTick: Box[FleaTick],
                 supplements: List[Product]
               ) = {

      SubscriptionService.saveNewPetProducts(updatedFleaTick, subscriptionBox, supplements)

      val updatedSubscription = parent.map(ParentService.updateStripeSubscriptionTotal)

      updatedSubscription match {
        case Full(_) =>
          selectedPet = Empty

          petsRenderer.map(_.setHtml()).openOr(Noop)
        case _ =>
          Alert("An error has occurred. Please try again.")
      }
    }

    def petProductsBindings = {
      "#product-picker-modal" #> idMemoize { renderer =>
        petProductsRender = Full(renderer)

        val box = selectedPet.flatMap(_.box.obj)
        val petType = selectedPet.map(_.animalType.get).openOrThrowException("Missing animal type")
        val availableFleaTick = SubscriptionService.getAvailableFleaTick(selectedPet)
        val availableSupplements = Product.allSupplements(petType)
        val currentSupplements = SubscriptionService.getCurrentSupplements(box)

        var currentFleaTick = box.flatMap(_.fleaTick.obj)
        var (firstSupplement, secondSupplement, thirdSupplement) =
          SubscriptionService.getFirstSecondThirdSupplements(currentSupplements)

        val zoGuardProducts = SubscriptionService.getZoGuardProductsAndCurrent(
          currentFleaTick,
          availableFleaTick
        )

        val currentProductDropdown = SHtml.ajaxSelectObj(
          zoGuardProducts.map(product => (product, product.getNameAndSize)),
          currentFleaTick,
          (possibleProduct: FleaTick) =>
            currentFleaTick = {
              Full(possibleProduct)
            }
        )

        val firstSupplementDropDown = SHtml.ajaxSelectObj(
          availableSupplements.map(product => (product, product.name.get)),
          firstSupplement,
          (possibleProduct: Product) =>
            firstSupplement = {
              Full(possibleProduct)
            }
        )

        val secondSupplementDropDown = SHtml.ajaxSelectObj(
          availableSupplements.map(product => (product, product.name.get)),
          secondSupplement,
          (possibleProduct: Product) =>
            secondSupplement = {
              Full(possibleProduct)
            }
        )

        val thirdSupplementDropDown = SHtml.ajaxSelectObj(
          availableSupplements.map(product => (product, product.name.get)),
          thirdSupplement,
          (possibleProduct: Product) =>
            thirdSupplement = {
              Full(possibleProduct)
            }
        )

        "^ [class+]" #> (if (!selectedPet.isEmpty) "active" else "") &
        ".modal-header .parent" #> ClearNodes &
        ".modal-header .admin .parent-name" #> parent.map(_.name) &
        ".supplement" #> ClearNodesIf(currentSupplements.isEmpty) andThen
        "#flea-tick" #> currentProductDropdown &
        "#first-supplement" #> firstSupplementDropDown &
        "#second-supplement" #> secondSupplementDropDown &
        "#third-supplement" #> thirdSupplementDropDown &
        ".save" #> ajaxSubmit(
          "Save",
          () =>
            savePet(
              box,
              currentFleaTick,
              List(firstSupplement, secondSupplement, thirdSupplement).flatten
            )
        ) &
        ".cancel" #> ajaxSubmit("Cancel", () => {
          selectedPet = Empty
          petProductsRender.map(_.setHtml()).openOr(Noop)
        })
      }
    }

    SHtml.makeFormsAjax andThen
    petProductsBindings andThen
    ".parent-pets" #> ClearNodesIf(isCancelled_?(parent)) &
    ".parent-pets" #> idMemoize { renderer =>
      petsRenderer = Full(renderer)
      ".create" #> {
          ".new-pet-name" #> ajaxText(petName, petName = _) &
          ".new-pet-breed" #> ajaxText(petBreed, petBreed = _) &
          ".new-pet-birthday" #> ajaxText(petBirthday, petBirthday = _) &
          ".pet-type-select" #> petTypeRadio(renderer) &
          ".product-container .product-select" #> productDropdown &
          ".create-item-container .create-item" #> SHtml.ajaxSubmit(
            "Add Pet",
            () => addPet(parent, renderer)
          )
        } &
      ".add-coupon" #> {
        ".coupon-container .coupon-select" #> couponDropdown &
          ".create-coupon-container .create-coupon" #> SHtml.ajaxSubmit(
            "Change Coupon",
            () => addCoupon(parent, chosenCoupon)
          )
      } &
      {
        val pets = Pet.findAll(
          By(Pet.user, parent),
          By(Pet.status, Status.Active)
        )

        ".pet" #> pets.map { pet =>
          val birthday        = tryo(birthdayFormat.format(pet.birthday.get)).map(_.toString).openOr("")
          var nextGrowthDelay = tryo(pet.nextGrowthDelay.get.toString).openOr("")
          val subscriptionBox = SubscriptionBox.find(By(SubscriptionBox.pet, pet))

          ".pet-name" #> SHtml.ajaxText(
            pet.name.get,
            possiblePetName => updatePetInfo(possiblePetName, "name", Full(pet))
          ) &
          ".pet-breed" #> SHtml.ajaxText(
            pet.breed.get,
            possibleBreed => updatePetInfo(possibleBreed, "breed", Full(pet))
          ) &
          ".pet-birthday *" #> SHtml.ajaxText(
            birthday,
            possibleBirthday => updatePetInfo(possibleBirthday, "birthday", Full(pet))
          ) &
          ".pet-type *" #> pet.animalType.toString &
          ".pet-delay-growth input" #> ajaxText(
            s"$nextGrowthDelay months",
            possibleDelay => updateGrowthDelay(possibleDelay, pet)
          ) &
          ".actions .show-pet-product [onClick]" #> ajaxInvoke(showProducts(Full(pet)) _) &
          ".actions .delete [onclick]" #> Confirm(
            s"Delete ${pet.name}?",
            ajaxInvoke(deletePet(parent, pet, renderer))
          )
        }
      }
    }
  }

  def shipmentBindings(
      detailsRenderer: IdMemoizeTransform,
      subscription: Option[Subscription]
  ): NodeSeq => NodeSeq = {
    val parent       = currentParent
    val nextShipDate = subscription.map(_.nextShipDate.get)

    val shipments: List[Shipment] = subscription.map { sub =>
      Shipment.findAll(By(Shipment.subscription, sub))
    }.getOrElse(Nil)

    var updateNextShipDate =
      nextShipDate.map(date => nextShipDateFormat.format(date)).getOrElse("")

    def updateShipDate() = {
      val updatedDate = nextShipDateFormat.parse(updateNextShipDate)

      subscription.map { oldSubscription =>
        ParentService.updateNextShipBillDate(oldSubscription, updatedDate)
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
        ".change-date [onClick]" #> SHtml.ajaxInvoke(() => updateShipDate) &
        ".shipment" #> shipments
          .sortWith(_.dateProcessed.get.getTime > _.dateProcessed.get.getTime)
          .map { shipment =>
            val itemsShipped = shipment.shipmentLineItems.toList.map(_.getPetNameProductName)

            ".paid-date *" #> tryo(dateFormat.format(shipment.dateProcessed.get)).openOr("-") &
              ".ship-date *" #> tryo(dateFormat.format(shipment.dateShipped.get)).openOr("-") &
              ".refund-date *" #> tryo(dateFormat.format(shipment.dateRefunded.get)).openOr("-") &
              ".amount-paid .stripe-invoice *" #> s"$$${shipment.amountPaid.get}" &
              ".amount-paid .stripe-invoice [href]" #> s"${stripeInvoiceBaseURL}/${shipment.stripePaymentId.get}" &
              ".pets ul" #> {
                itemsShipped.sortWith(_ < _).map { itemShipped => ".pet-product *" #> itemShipped }
              } &
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

  def actionLogBinding(
 ): CssBindFunc = {
    val parent = currentParent
    lazy val actions = ActionLogService.findActionsByParentId(parent.map(_.userId.get).openOr(0L))

    ".action" #> actions.sortBy( _.timestamp).reverse.map { action =>
      val actionDetails = action.details.stringDetails.map { case (key, value) =>
        s"${SplitTitleCase(key)}: $value"
      }.mkString(". ")

      val performedBy = {
        if (action.userId.contains(0))
          User.find(By(User.userId, action.parentId))
        else
          User.find(By(User.userId, action.userId.getOrElse(0L)))
      }

      ".action-date *" #> dateTimeFormat.format(Date.from(action.timestamp)) &
      ".action-subtype *" #> SplitTitleCase(action.actionSubtype.toString) &
      ".performed-by *" #> performedBy.map(_.name) &
      ".details *" #> actionDetails
    }
  }

  def render: NodeSeq => NodeSeq = {
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
              val subscription = parent.subscription.obj
              val nextShipDate = subscription.map(_.nextShipDate.get)

              ".parent" #> {
                ".name *" #> parent.name &
                  ".email *" #> parent.email.get &
                  ".billing-status *" #> subscription
                    .map(_.status.get.toString.split("(?=\\p{Upper})").mkString(" ")) &
                  ".referer *" #> refererName &
                  ".ship-date *" #> tryo(
                    displayNextShipDate(
                      nextShipDate.map(dateFormat.format(_)),
                      isCancelled_?(parent)
                    )
                  ).openOr("-") &
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
                ".info [class+]" #> { if (currentParent.isEmpty) "" else "expanded" } &
                "^ [class+]" #> { if (currentParent.isEmpty) "" else "expanded" } &
                ".parent-info" #> {
                  if (!currentParent.isEmpty) {
                    petBindings andThen
                    shipmentBindings(detailsRenderer, subscription) andThen
                    parentInformationBinding(detailsRenderer, subscription) andThen
                    subscriptionBinding(detailsRenderer, subscription) andThen
                    actionLogBinding()
                  } else {
                    "^" #> ClearNodes
                  }
                }
            }
          }
        }
  }
}
