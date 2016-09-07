package com.mypetdefense.snippet
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
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
import com.mypetdefense.util.ClearNodesIf

object Parents extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Parents") / "admin" / "parents" 
    //>>
    //adminUser >>
    //loggedIn
}

class Parents extends Loggable {
  val parents = User.findAll(By(User.userType, UserType.Parent))
  
  var petType: Box[AnimalType.Value] = Empty
  var chosenProduct: Box[Product] = Empty
  var petName = ""

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

    SHtml.selectObj(
      products.map(product => (product, product.getNameAndSize)),
      chosenProduct,
      (possibleProduct: Product) => chosenProduct = Full(possibleProduct)
    )
  }

  def addPet(parent: User) = {
    val validateFields = List(
      checkEmpty(petName, ".new-pet-name")
    ).flatten

    if(validateFields.isEmpty) {
      (for {
        pet <- petType
        product <- chosenProduct
        size = product.size.get
      } yield {
        Pet.createNewPet(
          user = parent,
          name = petName,
          animalType = pet,
          size = size,
          product = product
        )
      })
      Noop
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".parents [class+]" #> "current" &
    "tbody" #> parents.map { parent =>
      val dateFormat = new SimpleDateFormat("MMM dd")

      val nextShipDate = Subscription.find(By(Subscription.user, parent)).map(_.nextShipDate.get)

      ".parent" #> {
        ".name *" #> parent.name &
        ".email *" #> parent.email &
        ".phone *" #> parent.phone &
        ".coupon *" #> parent.coupon.obj.map(_.couponCode.get) &
        ".referer *" #> parent.referer.obj.map(_.name.get) &
        ".ship-date *" #> nextShipDate.map(dateFormat.format(_))
      } &
      ".pets" #> idMemoize { renderer =>
        ".create" #> {
          ".new-pet-name" #> ajaxText(petName, petName = _) &
          ".pet-type-select" #> petTypeRadio(renderer) &
          ".product-container .product-select" #> productDropdown &
          ".create-item-container .create-item" #> SHtml.ajaxSubmit("Add Pet", () => addPet(parent))
        } &
        ".pet" #> parent.pets.map { pet =>
          ".pet-name *" #> pet.name &
          ".pet-type *" #> pet.animalType.toString &
          ".pet-product *" #> pet.product.obj.map(_.getNameAndSize)
        }
      }
    }
  }
}

