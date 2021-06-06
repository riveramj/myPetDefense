package com.mypetdefense.snippet.signup

import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices.cart
import com.mypetdefense.service.ValidationService.checkEmpty
import com.mypetdefense.service._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.http.SHtml.{ajaxInvoke, ajaxText}
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import java.text.SimpleDateFormat
import scala.collection.mutable
import scala.xml.Elem

object PetDetails extends Loggable {
  import net.liftweb.sitemap._

  val menu = Menu.i("Pet Details") / "pet-details"
}

class PetDetails extends Loggable {
  val formatter          = new SimpleDateFormat("MM/yy")
  val yearMonthFormatter = new SimpleDateFormat("MMM-yyyy")

  var petDetailsRenderer: Box[IdMemoizeTransform]          = Empty
  var petPlansRenderer: Box[IdMemoizeTransform]            = Empty
  var chooseCareRenderer: Box[IdMemoizeTransform]          = Empty
  var careQuestionRenderer: Box[IdMemoizeTransform]        = Empty
  var detailsEnteredRenderer: Box[IdMemoizeTransform]      = Empty
  var currentPets: mutable.LinkedHashMap[Long, CheckoutPet] = cart.is
  var petName                        = ""
  var petMonth                       = ""
  var petYear                        = ""
  var petSize: Box[AnimalSize.Value] = Empty

  var chosenPetType: Box[AnimalType.Value]   = Empty
  var chosenSupplement: Box[Product]         = Empty
  var chosenPlanType: Box[BoxType.Value]     =  Full(BoxType.basic)
  var chosenPlanPrice: Box[Price]            =  Empty
  var availableBoxTypes: List[BoxType.Value] = Nil

  val products: List[FleaTick] = FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))

  val cat: Box[AnimalSize.Value]       = getSizeNumber(AnimalSize.CatAllSize)
  val smallDog: Box[AnimalSize.Value]  = getSizeNumber(AnimalSize.DogSmallZo)
  val mediumDog: Box[AnimalSize.Value] = getSizeNumber(AnimalSize.DogMediumZo)
  val largeDog: Box[AnimalSize.Value]  = getSizeNumber(AnimalSize.DogLargeZo)
  val xLargeDog: Box[AnimalSize.Value] = getSizeNumber(AnimalSize.DogXLargeZo)

  val supplements = Product.supplementsByAmount(30, AnimalType.Dog)

  val woofTraxOfferCode = S.param("oc")
  PetFlowChoices.woofTraxOfferCode(woofTraxOfferCode)
  PetFlowChoices.woofTraxUserId(S.param("ui"))

  if (woofTraxOfferCode.isDefined) {
    val coupon = Coupon.find(By(Coupon.couponCode, "80off"))
    PetFlowChoices.coupon(coupon)
  }

  def getSizeNumber(size: AnimalSize.Value): Option[AnimalSize.Value] = {
    products.find(_.size.get == size).map(_.size.get)
  }

  def validateNameBirthday: List[ValidationError] = {
    (
      checkEmpty(petName, "#pet-name") ::
        checkEmpty(petMonth, "#pet-month") ::
        checkEmpty(petYear, "#pet-year") ::
        checkEmpty(petSize.map(_.toString), ".pet-size") ::
        Nil
    ).flatten.distinct
  }

  def validateAndNavigate(url: String): JsCmd = {
    val validateFields = validateNameBirthday

    if (validateFields.isEmpty) {

    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def resetFields = {
    chosenPetType = Empty
    chosenPlanPrice = Empty
    chosenPlanType = Full(BoxType.basic)
    chosenSupplement = Empty
    petMonth = ""
    petYear = ""
  }

  def addToCart: JsCmd = {
    for {
      animalType <- chosenPetType
      boxType    <- chosenPlanType
      price <- chosenPlanPrice
    } yield {
      val birthday = yearMonthFormatter.parse(s"$petMonth-$petYear")

      val newPet = Pet.create
        .petId(generateLongId)
        .name(petName)
        .animalType(animalType)
        .birthday(birthday)
        .size(petSize.openOr(null))

      currentPets(newPet.petId.get) = CheckoutPet(PendingPet(newPet, boxType, chosenSupplement), price)
      cart(currentPets)
    }

    resetFields

    S.redirectTo(CartReview.menu.loc.calcDefaultHref)
  }

  def addNewPet(): JsCmd =
    validateAndNavigate(PetDetails.menu.loc.calcDefaultHref)

  def chooseSize(animalSize: Box[AnimalSize.Value]) = {
    petSize = animalSize
  }

  def choosePlan(boxType: BoxType.Value)() = {
    chosenPlanType = Full(boxType)

    findBoxPrice()

    petPlansRenderer.map(_.setHtml()).openOr(Noop)
  }

  def addSelectedClass(boxType: BoxType.Value) =
    if (chosenPlanType.contains(boxType))
      "selected"
    else
      ""

  def findBoxPrice() = {
    for {
      size <- petSize
      box <- chosenPlanType
      price <- Price.getDefaultProductPrice(size, box)
    } yield {
      chosenPlanPrice = Full(price)
    }
  }

  def showPlans = {
    availableBoxTypes = if (chosenPetType.contains(AnimalType.Dog))
      BoxType.dogBoxTypes
    else
      BoxType.catBoxTypes

    findBoxPrice()

    petPlansRenderer.map(_.setHtml()).openOr(Noop)
  }

  def monthDropdown: Elem = {
    SHtml.ajaxSelect(
      ("", "") +: List(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "November",
        "October",
        "November",
        "December"
      ).map(month => (month, month)),
      Full(petMonth),
      petMonth = _
    )
  }

  def yearDropdown: Elem = {
    SHtml.ajaxSelect(
      ("", "") +: List(
        "2021",
        "2020",
        "2019",
        "2018",
        "2017",
        "2016",
        "2015",
        "2014",
        "2013",
        "2012",
        "2011",
        "2010",
        "2009",
        "2008",
        "2007",
        "2006",
        "2005",
        "2004",
        "2003"
      ).map(year => (year, year)),
      Full(petYear),
      petYear = _
    )
  }

  def productDropdown = {
    SHtml.ajaxSelectObj(
      supplements.map(supplement => (Full(supplement), supplement.nameAndQuantity)),
      Full(chosenSupplement),
      (possibleSupplement: Box[Product]) => chosenSupplement = possibleSupplement
    )
  }

  def getPetNameOrType = {
    if(petName.isEmpty)
      s"your ${chosenPetType.map(_.toString.toLowerCase).openOr("pet")}"
    else
      petName
  }

  def clearIfUnavailable(boxType: BoxType.Value) =
    ClearNodesIf(!availableBoxTypes.contains(boxType))

  def render = {
    resetFields

    def setPetFlow(animalType: AnimalType.Value) = {
      chosenPetType = Full(animalType)

      petDetailsRenderer.map(_.setHtml()).openOr(Noop)
    }

    ".pet-selection" #> {
      "#dog [onclick]" #> SHtml.ajaxInvoke(() => setPetFlow(AnimalType.Dog)) &
      "#cat [onclick]" #> SHtml.ajaxInvoke(() => setPetFlow(AnimalType.Cat)) &
      ".details-entered" #> SHtml.idMemoize { renderer =>
        detailsEnteredRenderer = Full(renderer)

        ".given-pet-name *" #> getPetNameOrType
      }
    } &
    ".pet-details-container" #> SHtml.idMemoize { renderer =>
      petDetailsRenderer = Full(renderer)
      val petMissing = chosenPetType.isEmpty

      ".pet-details [class+]" #> (if (petMissing) "pet-missing" else "") andThen
      ".pet [class+]" #> chosenPetType.map(_.toString.toLowerCase) &
        ".pet-type *" #> chosenPetType.map(_.toString) &
        "#pet-name" #> ajaxText(petName, possibleName => {
          petName = possibleName

          (
            chooseCareRenderer.map(_.setHtml()).openOr(Noop) &
              detailsEnteredRenderer.map(_.setHtml()).openOr(Noop) &
              careQuestionRenderer.map(_.setHtml()).openOr(Noop)
            )
        }) &
        "#month-container #pet-month" #> monthDropdown &
        "#year-container #pet-year" #> yearDropdown &
        ".small-dog .weight-number *" #> smallDog.map(_.toString + " lb") &
        ".small-dog #small-dog [onclick]" #> ajaxInvoke(() => chooseSize(smallDog)) &
        ".medium-dog .weight-number *" #> mediumDog.map(_.toString + " lb") &
        ".medium-dog #medium-dog [onclick]" #> ajaxInvoke(() => chooseSize(mediumDog)) &
        ".large-dog .weight-number *" #> largeDog.map(_.toString + " lb") &
        ".large-dog #large-dog [onclick]" #> ajaxInvoke(() => chooseSize(largeDog)) &
        ".xlarge-dog .weight-number *" #> xLargeDog.map(_.toString + " lb") &
        ".xlarge-dog #xlarge-dog [onclick]" #> ajaxInvoke(() => chooseSize(xLargeDog)) &
        ".chose-care-container" #> SHtml.idMemoize { renderer =>
          chooseCareRenderer = Full(renderer)

          ".given-pet-name *" #> getPetNameOrType &
          "#choose-care [onclick]" #> ajaxInvoke(() => showPlans)
        } &
        ".care-question" #> SHtml.idMemoize { renderer =>
          careQuestionRenderer = Full(renderer)

          ".given-pet-name *" #> getPetNameOrType
        }

    } &
    ".pet-plans" #> SHtml.idMemoize { renderer =>
      petPlansRenderer = Full(renderer)

      "^" #> ClearNodesIf(petSize.isEmpty) andThen
      ".everyday-plan" #> clearIfUnavailable(BoxType.everyday) andThen
      ".complete-plan" #> clearIfUnavailable(BoxType.complete) andThen
      ".given-pet-name *" #> getPetNameOrType &
      ".plan-prices" #> {
        "#choose-basic-plan" #> {
          "^ [onclick]" #> ajaxInvoke(choosePlan(BoxType.basic)) &
          "^ [class+]" #> addSelectedClass(BoxType.basic)
        } &
        "#choose-everyday-plan" #> {
          "^ [onclick]" #> ajaxInvoke(choosePlan(BoxType.everyday)) &
          "^ [class+]" #> addSelectedClass(BoxType.everyday)
        } &
        "#choose-complete-plan" #> {
          "^ [onclick]" #> ajaxInvoke(choosePlan(BoxType.complete)) &
          "^ [class+]" #> addSelectedClass(BoxType.complete)
        }
      } &
      ".selected-plan-info" #> {
        ".price span *" #> chosenPlanPrice.map(_.price.get.toString) &
        ".tag-line *" #> chosenPlanType.map(_ + " tagline")
      } &
      "#choose-plan [onclick]" #> ajaxInvoke(addToCart _)
    }
  }
}
