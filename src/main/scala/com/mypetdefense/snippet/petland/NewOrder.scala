package com.mypetdefense.snippet
package petland

import net.liftweb.sitemap.Menu
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

import com.mypetdefense.util.{ClearNodesIf, SecurityContext}
import com.mypetdefense.model._
import com.mypetdefense.actor._

import java.util.Date
import java.time.MonthDay
import java.time.{LocalDate, ZoneId}
import java.text.SimpleDateFormat

import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import scala.concurrent.Await
import scala.concurrent.duration._

import me.frmr.stripe.{StripeExecutor, Customer, Coupon => StripeCoupon, Subscription => StripeSubscription, Product => _, _}

import dispatch._, Defaults._


object petsOrdered extends SessionVar[List[Pet]](Nil)

case class OrderSubmitted(email: String) extends MyPetDefenseEvent("order-submitted")

case class PetAdded(petName: String) extends MyPetDefenseEvent("pet-added")

object NewOrder extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Petland New Order") / "petland" / "new-order" >>
    agentOrAdmin >>
    loggedIn
}

class NewOrder extends Loggable {
  val stripeSecretKey = Props.get("secret.key") openOr ""
  implicit val e = new StripeExecutor(stripeSecretKey)

  val currentUser = SecurityContext.currentUser
  
  val monthsOfyear = List(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )

  val dogZoguardProduct = FleaTick.findAll(
    By(FleaTick.name, "ZoGuard Plus for Dogs")
  )

  val catZoguardProduct = FleaTick.findAll(
    By(FleaTick.size, AnimalSize.CatAllSize)
  )

  var pets = petsOrdered.is
  var email = ""
  var phone = ""
  var firstName = ""
  var lastName = ""
  var street1 = ""
  var street2 = ""
  var city = ""
  var state = ""
  var zip = ""
  var taxRate = 0D
  var taxDue = 0D
  var subtotal = 0D
  var total = 0D
  
  var newPetType: Box[AnimalType.Value] = Empty
  var newPetCurrentSize: Box[AnimalSize.Value] = Empty
  var newPetAdultSize: Box[AnimalSize.Value] = Empty
  
  var birthdayMonth = ""
  var birthdayYear = ""
  var petName = ""

  var stripeToken = ""

  var addPetRenderer: Box[IdMemoizeTransform] = Empty
  var orderDetailsRenderer: Box[IdMemoizeTransform] = Empty
  var totalsRenderer: Box[IdMemoizeTransform] = Empty

  val birthdayDateFormat = new SimpleDateFormat("MMM yyyy")

  val petlandPlan = ParentService.getCurrentPetlandProductPlan
  val petlandPlanId = petlandPlan.map(_.id).openOr("")

  def calculateTax(possibleState: String, possibleZip: String) = {
    state = possibleState
    zip = possibleZip

    val taxInfo = TaxJarService.findTaxAmoutAndRate(
      city,
      state,
      zip,
      subtotal
    )

    taxDue = taxInfo._1
    taxRate = taxInfo._2

    totalsRenderer.map(_.setHtml).openOr(Noop)
  }

  def signup() = {
    val validateFields = List(
        checkEmail(email, "#email"),
        checkEmpty(phone, "#phone"),
        checkEmpty(firstName, "#first-name"),
        checkEmpty(lastName, "#last-name"),
        checkEmpty(street1, "#street-1"),
        checkEmpty(city, "#city"),
        checkEmpty(state, "#state"),
        checkEmpty(zip, "#zip")
      ).flatten

    if(validateFields.isEmpty) {
      val stripeCustomer = {
        Customer.create(
          email = Some(email),
          card = Some(stripeToken),
          taxPercent = Some(taxRate),
          plan = Some(petlandPlanId),
          quantity = Some(pets.size)
        )
      }

      Try(Await.result(stripeCustomer, new DurationInt(7).seconds)) match {
        case TrySuccess(Full(customer)) =>
          val user = newUserSetup(customer)

          EmailActor ! SendNewUserEmail(user)
          EmailActor ! Send6MonthSaleReceipt(user, pets, subtotal, taxDue)

          pets = Nil
          petsOrdered(Nil)
          email = ""
          phone = ""
          firstName = ""
          lastName = ""
          street1 = ""
          street2 = ""
          city = ""
          state = ""
          zip = ""

          OrderSubmitted(user.email.get)

        case TrySuccess(stripeFailure) =>
          logger.error(s"create customer failed with stripe error: ${stripeFailure}")
          Alert("An error has occured. Please try again. If you continue to receive an error, please contact us at help@mypetdefense.com.")

        case TryFail(throwable: Throwable) =>
          logger.error(s"create customer failed with other error: ${throwable}")
          Alert("An error has occured. Please try again. If you continue to receive an error, please contact us at help@mypetdefense.com.")
      }
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def newUserSetup(customer: Customer) = {
    val stripeId = customer.id

    val newParent = User.createNewUser(
      firstName = firstName,
      lastName = lastName,
      stripeId = stripeId,
      email = email,
      password = "",
      phone = phone,
      coupon = None,
      referer = currentUser.flatMap(_.agency.obj),
      agency = None,
      userType = UserType.Parent
    )

    Address.createNewAddress(
      Full(newParent),
      street1,
      street2,
      city,
      state,
      zip,
      AddressType.Shipping
    )
    
    createNewPets(newParent)

    val subscriptionId = (
      for {
        rawSubscriptions <- customer.subscriptions
        subscription <- rawSubscriptions.data.headOption
      } yield {
        subscription.id
      }).flatMap(identity).getOrElse("")

    Subscription.createNewSubscription(
      newParent,
      subscriptionId,
      new Date(),
      new Date(),
      Price.currentPetlandMonthlyCode,
      6
    )

    /*
    if (Props.mode == Props.RunModes.Production) {
      EmailActor ! NewSaleEmail(newParent, cart.size, coupon.map(_.couponCode.get).openOr(""))
    }
    */

    newParent
  }

  def createNewPets(parent: User) = {
    pets.map { pet =>
      Pet.createNewPet(pet, parent)
    }
  }

  def petTypeDropdown(renderer: IdMemoizeTransform) = {
    SHtml.ajaxSelectObj(
      List(
        (Empty, "Choose Pet"),
        (Full(AnimalType.Cat), AnimalType.Cat.toString),
        (Full(AnimalType.Dog), AnimalType.Dog.toString)
      ),
      Full(newPetType),
      (possiblePetType: Box[AnimalType.Value]) => {
        newPetType = possiblePetType
        renderer.setHtml
      }
    )
  }

  def petSizeDropdown(petSize: String) = {
    val products = newPetType.map { animal =>
      if (animal == AnimalType.Cat)
        catZoguardProduct
      else
        dogZoguardProduct
    }.openOr(Nil)

    val productChoices = products.map(product => (Full(product.size.get), product.getSizeAndSizeName))

    newPetCurrentSize = productChoices.headOption.flatMap(_._1)
    newPetAdultSize = productChoices.headOption.flatMap(_._1)

    SHtml.ajaxSelectObj(
      productChoices,
      Full(newPetCurrentSize),
      (possibleSize: Box[AnimalSize.Value]) => {
        if (petSize == "current") {
          newPetCurrentSize = possibleSize
        } else {
          newPetAdultSize = possibleSize
        }
      }
    )
  }

  def birthdayMonthDropdown = {
    SHtml.ajaxSelect(
      List(("", "Month")) ++ monthsOfyear.map(month => (month, month)),
      Full(birthdayMonth),
      birthdayMonth = _
    )
  }
  
  def birthdayYearDropdown = {
    SHtml.ajaxSelect(
      List(("", "Year")) ++ ((2019 to 1998 by -1).toList.map(year => (year.toString, year.toString))),
      Full(birthdayYear),
      birthdayYear = _
    )
  }

  def addPet() = {
    val product = {
      if (newPetType == Full(AnimalType.Cat)) {
        catZoguardProduct
      } else {
        dogZoguardProduct.filter { product =>
          Full(product.size.get) == newPetCurrentSize
        }
      }
    }.headOption

    val birthday = tryo(birthdayDateFormat.parse(s"$birthdayMonth $birthdayYear"))

    val newPet = { 
      for {
        animal <- newPetType.toList
        currentSize <- newPetCurrentSize
        adultSize <- newPetAdultSize
        neededProduct <- product
      } yield {
        val realPetName = {
          if (petName == "")
            s"Pet ${pets.size + 1}"
          else
            petName
        }

        val possiblePet = {
          Pet.create
            .name(realPetName)
            .animalType(animal)
            .size(currentSize)
            .adultSize(adultSize)
            .fleaTick(neededProduct)
        }

        if (birthday.isEmpty)
          Full(possiblePet)
        else
          birthday.map(possiblePet.birthday(_))
      }
    }.toList.flatten

    pets = pets ++ newPet
    petsOrdered(pets)

    newPetType = Empty
    newPetCurrentSize = Empty
    newPetAdultSize = Empty

    birthdayMonth = ""
    birthdayYear = ""
    petName = ""


    (
      addPetRenderer.map(_.setHtml).openOr(Noop) &
      orderDetailsRenderer.map(_.setHtml).openOr(Noop) &
      totalsRenderer.map(_.setHtml).openOr(Noop) &
      PetAdded(newPet.map(_.name.get).headOption.getOrElse(""))
    )
  }

  def removePet(pet: Pet) = {
    pets = pets.filter(_ != pet)

    petsOrdered(pets)

    (
      orderDetailsRenderer.map(_.setHtml).openOr(Noop) &
      totalsRenderer.map(_.setHtml).openOr(Noop)
    )
  }

  def addPetBindings = {
    "#new-pet" #> SHtml.idMemoize { renderer =>
      addPetRenderer = Full(renderer)

      "#pet-type" #> petTypeDropdown(renderer) &
      "#current-size" #> petSizeDropdown("current") &
      "#adult-size" #> petSizeDropdown("adult") &
      "#birthday-month" #> birthdayMonthDropdown &
      "#birthday-year" #> birthdayYearDropdown &
      "#new-pet-name" #> text(petName, petName = _) &
      "#add-to-order" #> SHtml.ajaxSubmit("Add to Order", () => addPet)
    }
  }

  def orderBindings = {
    ".subscription-details" #> idMemoize { renderer =>
      orderDetailsRenderer = Full(renderer)
      ".pet-entry" #> pets.map { pet =>
        val birthday = tryo(birthdayDateFormat.format(pet.birthday.get))

        ".pet-name *" #> pet.name.get &
        ".pet-birthday *" #> birthday &
        ".pet-current-product *" #> pet.fleaTick.obj.map(_.getNameAndSize) &
        ".remove [onclick]" #> ajaxInvoke(() => removePet(pet))
      }
    } &
    "#empty-cart [class+]" #> { 
      if (pets.size > 0)
        "hidden"
      else
        ""
    } &
    ".continue-shopping *" #> (if (pets.size > 0) "Continue Shopping" else "Add Another Pet")
  }

  def totalSummaryBindings = {
    ".order-totals" #> idMemoize { renderer =>
      totalsRenderer = Full(renderer)

      subtotal = pets.size.toDouble * 90.00

      total = subtotal + taxDue

      ".subtotal-amount *" #> f"$$$subtotal%2.2f" &
      ".tax-amount *" #> f"$$$taxDue%2.2f" &
      ".total-price *" #> f"$$$total%2.2f"
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".new-order [class+]" #> "current" &
    ".overview" #> ClearNodesIf(!SecurityContext.admin_?) &
    addPetBindings &
    orderBindings &
    totalSummaryBindings &
    "#first-name" #> text(firstName, firstName = _) &
    "#last-name" #> text(lastName, lastName = _) &
    "#street-1" #> text(street1, street1 = _) &
    "#street-2" #> text(street2, street2 = _) &
    "#city" #> ajaxText(city, city = _) &
    "#state" #> ajaxText(state, possibleState => calculateTax(possibleState, zip)) &
    "#zip" #> ajaxText(zip, possibleZip => calculateTax(state, possibleZip)) &
    "#phone" #> ajaxText(phone, phone = _) &
    "#email" #> text(email, userEmail => email = userEmail.trim) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    ".submit" #> SHtml.ajaxSubmit("Submit", () => signup)
  }
}
