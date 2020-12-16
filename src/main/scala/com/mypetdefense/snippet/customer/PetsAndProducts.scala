package com.mypetdefense.snippet.customer

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.SecurityContext._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.{Elem, NodeSeq}

object PetsAndProducts extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Pets and Products") / "pets-products" >>
    loggedIn >>
    parent
}

class PetsAndProducts extends Loggable {
  val user: Box[User]                 = currentUser
  val subscription: Box[Subscription] = user.flatMap(_.subscription.obj)
  val priceCode: String               = subscription.map(_.priceCode.get).getOrElse("")

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

  def productDropdown(): Elem = {
    val products =
      if (newPetType.map(_.equals(AnimalType.Dog)).openOr(true))
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
        ParentService.addNewPet(
          oldUser = parent,
          name = newPetName,
          animalType = pet,
          size = size,
          product = product,
          isUpgraded = parent.subscription.obj.map(_.isUpgraded.get).openOr(false)
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
    ParentService.removePet(user, pet) match {
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
    selectedPet = pet

    petProductsRender.map(_.setHtml()).openOr(Noop)
  }

  def savePet(
      subscriptionBox: Box[SubscriptionBox],
      updatedFleaTick: Box[FleaTick],
      supplements: List[Product]
  ) = {
    for {
      fleaTick <- updatedFleaTick
      box      <- subscriptionBox
    } yield {
      box.fleaTick(fleaTick).saveMe

      val items = box.subscriptionItems.toList.filterNot(_.product.obj == Product.dentalPowder)

      for {
        index <- items.indices
      } yield {
        items(index).product(supplements(index)).saveMe()
      }
    }

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
      val products =
        if (selectedPet.map(_.animalType.get.equals(AnimalType.Dog)).openOr(true))
          FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog))
        else
          FleaTick.zoGuardCat.toList

      val availableSupplements = Product.findAll().filterNot(Full(_) == Product.dentalPowder)

      var currentProduct = box.flatMap(_.fleaTick.obj)
      val currentSupplements = {
        for {
          currentBox       <- box.toList
          subscriptionItem <- currentBox.subscriptionItems.toList
          product          <- subscriptionItem.product.obj.toList
          if Full(product) != Product.dentalPowder
        } yield {
          product
        }
      }

      var firstSupplement = currentSupplements.headOption
      var secondSupplement =
        if (currentSupplements.nonEmpty) currentSupplements.tail.headOption else None
      var thirdSupplement = currentSupplements.reverse.headOption

      val zoGuardProducts =
        if (currentProduct.map(_.isZoGuard_?).openOr(false))
          products.filter(_.isZoGuard_?)
        else
          currentProduct.toList ++ products.filter(_.isZoGuard_?)

      val currentProductDropdown = SHtml.ajaxSelectObj(
        zoGuardProducts.map(product => (product, product.getNameAndSize)),
        currentProduct,
        (possibleProduct: FleaTick) =>
          currentProduct = {
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
                currentProduct,
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
      parent.subscription.obj.map(_.subscriptionBoxes.toList)
    }.openOr(Nil)

    SHtml.makeFormsAjax andThen
      petProductsBindings andThen
      ".pets-products a [class+]" #> "current" &
        "#user-email *" #> user.map(_.email.get) &
        "#new-pet" #> idMemoize { renderer =>
          "#new-pet-name" #> ajaxText(newPetName, newPetName = _) &
            "#pet-type-select" #> petTypeDropdown(renderer) &
            "#new-pet-product-select" #> productDropdown() &
            "#add-pet" #> SHtml.ajaxSubmit("Add Pet", () => addPet)
        } &
        ".pet" #> boxes.map { box =>
          val pet            = box.pet.obj
          var currentPetName = pet.map(_.name.get).openOr("")
          val product        = box.fleaTick.obj

          val price = if (SubscriptionBox.possiblePrice(box) == 0d) {
            product.flatMap { item => Price.getPricesByCode(item, priceCode).map(_.price.get) }
              .openOr(0d)
          } else {
            SubscriptionBox.possiblePrice(box)
          }

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
