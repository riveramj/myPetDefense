package com.mypetdefense.snippet.signup

import com.mypetdefense.model._
import com.mypetdefense.service.PetFlowChoices._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.service._
import com.mypetdefense.util.RandomIdGenerator.generateLongId
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import java.text.SimpleDateFormat
import scala.collection.mutable
import scala.xml.{Elem, NodeSeq}

object DogDetails extends Loggable {
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Dog Details") / "dog-details"
}

class DogDetails extends Loggable {
  val woofTraxOfferCode = S.param("oc")
  PetFlowChoices.woofTraxOfferCode(woofTraxOfferCode)
  PetFlowChoices.woofTraxUserId(S.param("ui"))

  if (woofTraxOfferCode.isDefined) {
    val coupon = Coupon.find(By(Coupon.couponCode, "80off"))
    PetFlowChoices.coupon(coupon)
  }
  
  petChoice(Full(AnimalType.Dog))

  if (petId.is.isEmpty)
    petId(Full(generateLongId))

  val formatter          = new SimpleDateFormat("MM/yy")
  val yearMonthFormatter = new SimpleDateFormat("MMM-yyyy")

  var currentPets: mutable.LinkedHashMap[Long, PendingPet] = completedPets.is
  var petName                                       = ""
  var petMonth                                      = ""
  var petYear                                       = ""
  var petSize: Box[AnimalSize.Value]                = Empty
  var nameErrors: List[String]                      = Nil
  var chosenSupplement: Box[Product]                = Product.multiVitaminForDogs(30)

  val products: List[FleaTick] = FleaTick.findAll(By(FleaTick.name, "ZoGuard Plus for Dogs"))

  val smallDog: Option[AnimalSize.Value]  = getSizeNumber(AnimalSize.DogSmallZo)
  val mediumDog: Option[AnimalSize.Value] = getSizeNumber(AnimalSize.DogMediumZo)
  val largeDog: Option[AnimalSize.Value]  = getSizeNumber(AnimalSize.DogLargeZo)
  val xlargeDog: Option[AnimalSize.Value] = getSizeNumber(AnimalSize.DogXLargeZo)

  val supplements = Product.supplementsByAmount(30, AnimalType.Dog)

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
      for {
        newPetId   <- PetFlowChoices.petId.is
        animalType <- PetFlowChoices.petChoice.is
        boxType <- PetFlowChoices.petBoxType.is
      } yield {
        val birthday = yearMonthFormatter.parse(s"$petMonth-$petYear")

        val newPet = Pet.create
          .petId(newPetId)
          .name(petName)
          .animalType(animalType)
          .birthday(birthday)
          .size(petSize.openOr(null))

        currentPets(newPetId) = PendingPet(newPet, boxType, chosenSupplement)
        completedPets(currentPets)
      }

      petChoice(Empty)
      petId(Empty)

      S.redirectTo(url)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def goToCheckout(): JsCmd =
    validateAndNavigate(Checkout.menu.loc.calcDefaultHref)

  def addNewPet(): JsCmd =
    validateAndNavigate(PetChoice.menu.loc.calcDefaultHref)

  def chooseSize(animalSize: Option[AnimalSize.Value]): Unit = {
    petSize = animalSize
  }

  def monthDropdown: Elem = {
    SHtml.select(
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
    SHtml.select(
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
    SHtml.selectObj(
      supplements.map(supplement => (Full(supplement), supplement.nameAndQuantity)),
      Full(chosenSupplement),
      (possibleSupplement: Box[Product]) => chosenSupplement = possibleSupplement
    )
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      "#pet-name" #> ajaxText(petName, petName = _) &
        "#month-container #pet-month" #> monthDropdown &
        "#year-container #pet-year" #> yearDropdown &
        "#supplement-container #pet-supplement" #> productDropdown &
        ".small-dog .weight-number *" #> smallDog.map(_.toString + " lb") &
        ".small-dog #small-dog [onclick]" #> ajaxInvoke(() => chooseSize(smallDog)) &
        ".medium-dog .weight-number *" #> mediumDog.map(_.toString + " lb") &
        ".medium-dog #medium-dog [onclick]" #> ajaxInvoke(() => chooseSize(mediumDog)) &
        ".large-dog .weight-number *" #> largeDog.map(_.toString + " lb") &
        ".large-dog #large-dog [onclick]" #> ajaxInvoke(() => chooseSize(largeDog)) &
        ".xlarge-dog .weight-number *" #> xlargeDog.map(_.toString + " lb") &
        ".xlarge-dog #xlarge-dog [onclick]" #> ajaxInvoke(() => chooseSize(xlargeDog)) &
        "#add-pet" #> ajaxSubmit("Add Another Dog", addNewPet _) &
        "#checkout" #> ajaxSubmit("Add & Checkout", goToCheckout _)
  }
}
