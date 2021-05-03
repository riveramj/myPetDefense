package com.mypetdefense.snippet.customer

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.model.domain.action.CustomerAction.{CustomerAddedPet, CustomerRemovedPet}
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.snippet.login.Login
import com.mypetdefense.util.SecurityContext._
import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.sitemap.Loc.{IfValue, MatchWithoutCurrentValue}
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.{Elem, NodeSeq}

object PetsAndProducts extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Pets and Products") / "pets-products" >>
    loggedIn >>
    parent

  val preBillingMagicLink: Menu.ParamMenuable[User] =
    Menu.param[User](
      "PreBilling",
      "PreBilling",
      accessKey => KeyService.findUserByKey(accessKey, "preBillingKey"),
      user => user.accessKey.get
    ) / "pre-billing" >>
      MatchWithoutCurrentValue >>
      IfValue(_.isDefined, () => {
        RedirectResponse(Login.menu.loc.calcDefaultHref)
      })
}

class PetsAndProducts extends Loggable {
  val user: Box[User]                 = currentUser.map(_.reload)
  val subscription: Box[Subscription] = user.flatMap(_.subscription.obj)
  val upgradedSubscription = subscription.map(_.isUpgraded.get).openOr(false)
  val priceCode: String               = subscription.map(_.priceCode.get).getOrElse("")
  val monthlyDogSupplements           = Product.supplementsByAmount(30, AnimalType.Dog)
  var chosenNewPetSupplement: Box[Product]  = Empty

  var newPetType: Box[AnimalType.Value]  = Empty
  var newPetChosenProduct: Box[FleaTick] = Empty
  var newPetName                         = ""

  var petProductsRender: Box[IdMemoizeTransform] = Empty

  var selectedPet: Box[Pet] = Empty

  def petTypeDropdown(renderer: IdMemoizeTransform): Elem = {
    SHtml.ajaxSelectObj(
      List(
        (Empty, "Choose Pet"),
        (Full(AnimalType.Dog), AnimalType.Dog.toString),
        (Full(AnimalType.Cat), AnimalType.Cat.toString)
      ),
      Full(newPetType),
      (possiblePetType: Box[AnimalType.Value]) => {
        newPetType = possiblePetType
        renderer.setHtml
      }
    )
  }

  def newPetSupplementDropdown() = {
    val firstSupplementDropDown = SHtml.ajaxSelectObj(
      (Empty, "Choose Supplement") +: monthlyDogSupplements.map(product => (Full(product), product.nameAndQuantity)),
      Full(chosenNewPetSupplement),
      (possibleProduct: Box[Product]) =>
        chosenNewPetSupplement = possibleProduct
    )

    "^" #> ClearNodesIf(newPetType != AnimalType.Dog && subscription.map(_.isUpgraded.get).openOr(false)) &
    "#choose-supplement #new-pet-choose-supplement" #> firstSupplementDropDown
  }

  def productDropdown(): Elem = {
    val products =
      if (newPetType.isEmpty)
        Nil
      else if (newPetType.contains(AnimalType.Dog))
        FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog))
      else
        FleaTick.zoGuardCat.toList

    val zoGuardProducts = products.filter(_.isZoGuard_?)

    SHtml.ajaxSelectObj(
      (Empty, "Choose Product") +: zoGuardProducts.map(product =>
        (Full(product), product.getNameAndSize)
      ),
      Full(newPetChosenProduct),
      (possibleProduct: Box[FleaTick]) => newPetChosenProduct = possibleProduct
    )
  }

  def addPet: JsCmd = {
    val validateFields = List(
      checkEmpty(newPetName, "#new-pet-name")
    ).flatten

    if (validateFields.isEmpty) {
      (for {
        parent  <- user
        pet     <- newPetType
        product <- newPetChosenProduct
        size = product.size.get
      } yield {
        val actionLog = CustomerAddedPet(
          SecurityContext.currentUserId,
          Some(SecurityContext.currentUserId),
          0L,
          newPetName
        )

        ParentService.addNewPet(
          oldUser = parent,
          name = newPetName,
          animalType = pet,
          size = size,
          product = product,
          isUpgraded = parent.subscription.obj.map(_.isUpgraded.get).openOr(false),
          actionLog = Left(actionLog),
          chosenMonthlySupplement = chosenNewPetSupplement
        )
      }).flatMap(identity) match {
        case Full(pet) =>
          if (Props.mode != Props.RunModes.Pilot) {
            user.map { parent => EmailActor ! NewPetAddedEmail(parent, pet) }
          }

          S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
        case _ =>
          Alert("An error has occured. Please try again.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deletePet(pet: Box[Pet])(): Alert = {
    val actionLog = CustomerRemovedPet(
      SecurityContext.currentUserId,
      Some(SecurityContext.currentUserId),
      pet.map(_.petId.get).openOr(0),
      pet.map(_.name.get).openOr("-")
    )

    ParentService.removePet(user, pet, actionLog) match {
      case Full(_) =>
        if (Props.mode != Props.RunModes.Pilot) {
          user.map { parent => pet.map(p => EmailActor ! PetRemovedEmail(parent, p)) }
        }

        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occured. Please try again.")
    }
  }

  def showProducts(pet: Box[Pet])(): JsCmd = {
    selectedPet = pet.map(_.reload)

    petProductsRender.map(_.setHtml()).openOr(Noop)
  }

  def savePet(
      subscriptionBox: Box[SubscriptionBox],
      updatedFleaTick: Box[FleaTick],
      supplements: List[Product]
  ) = {
    SubscriptionService.saveNewPetProducts(updatedFleaTick, subscriptionBox, supplements)

    val updatedSubscription = currentUser.flatMap(ParentService.updateStripeSubscriptionTotal)

    updatedSubscription match {
      case Full(_) =>
        selectedPet = Empty

        S.redirectTo(PetsAndProducts.menu.loc.calcDefaultHref)
      case _ =>
        Alert("An error has occurred. Please try again.")
    }
  }

  def savePetName(pet: Box[Pet], name: String) = {
    pet.map(_.name(name).saveMe())

    Alert("Name has been updated.")
  }

  def petProductsBindings = {
    "#product-picker-modal" #> idMemoize { renderer =>
      petProductsRender = Full(renderer)

      val box = selectedPet.flatMap(_.box.obj)
      val availableFleaTick = SubscriptionService.getAvailableFleaTick(selectedPet)
      val petType = selectedPet.map(_.animalType.get)
      val monthSupply = box.map(_.monthSupply.get).openOr(false)
      val supplementCount = if (monthSupply) 30 else 10

      val availableSupplements = petType.toList.flatMap(pt => Product.supplementsByAmount(supplementCount, pt))
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
        (Empty, "") +: availableSupplements.map(product => (Full(product), product.nameAndQuantity)),
        Full(firstSupplement),
        (possibleProduct: Box[Product]) =>
          firstSupplement = {
            possibleProduct
          }
      )

      val secondSupplementDropDown = SHtml.ajaxSelectObj(
        (Empty, "") +: availableSupplements.map(product => (Full(product), product.nameAndQuantity)),
        Full(secondSupplement),
        (possibleProduct: Box[Product]) => secondSupplement = possibleProduct
      )

      val thirdSupplementDropDown = SHtml.ajaxSelectObj(
        (Empty, "") +: availableSupplements.map(product => (Full(product), product.nameAndQuantity)),
        Full(thirdSupplement),
        (possibleProduct: Box[Product]) => thirdSupplement = possibleProduct
      )

      "^ [class+]" #> (if (!selectedPet.isEmpty) "active" else "") &
      ".modal-header .admin" #> ClearNodes &
      ".supplement" #> ClearNodesIf(currentSupplements.isEmpty) andThen
      ".second-choice" #> ClearNodesIf(monthSupply) andThen
      ".third-choice" #> ClearNodesIf(monthSupply) andThen
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

  def render: NodeSeq => NodeSeq = {
    val boxes: List[SubscriptionBox] = user.flatMap { parent =>
      parent.subscription.obj.map(_.subscriptionBoxes.toList.filter(_.status.get == Status.Active))
    }.openOr(Nil)

    SHtml.makeFormsAjax andThen
    petProductsBindings andThen
    ".pets-products a [class+]" #> "current" &
    "#user-email *" #> user.map(_.email.get) &
    "#new-pet" #> idMemoize { renderer =>
      "#new-pet-name" #> ajaxText(newPetName, newPetName = _) &
      "#pet-type-select" #> petTypeDropdown(renderer) &
      "#new-pet-product-select" #> productDropdown() &
      "#choose-supplement" #> newPetSupplementDropdown() &
      "#add-pet" #> SHtml.ajaxSubmit("Add Pet", () => addPet)
    } &
    ".pet" #> boxes.map { box =>
      val pet            = box.pet.obj
      var currentPetName = pet.map(_.name.get).openOr("")
      val product        = box.fleaTick.obj
      val price = SubscriptionBox.findBoxPrice(box)

      ".pet-name" #> ajaxText(currentPetName, currentPetName = _) &
      ".price *" #> f"$$$price%2.2f" &
      ".pet-status *" #> pet.map(_.status.get.toString) &
      ".show-pet-product [onClick]" #> ajaxInvoke(showProducts(pet) _) &
      ".cancel [onclick]" #> Confirm(
        s"Remove $currentPetName and cancel future shipments?",
        ajaxInvoke(deletePet(pet) _)
      ) &
      ".save" #> ajaxSubmit("Save", () => savePetName(pet, currentPetName))
    }
  }
}
