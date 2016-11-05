package com.mypetdefense.snippet

import net.liftweb.sitemap.Menu
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http.{S, SHtml}
import net.liftweb.mapper.By

import com.mypetdefense.service.PetFlowChoices
import com.mypetdefense.model._

object DogSize extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Dog Size") / "dog-size" >>
    petChosen >>  
    dogProductChosen
}

class DogSize extends Loggable {
  import PetFlowChoices._

  def chosenSize = ".chosen-size *" #> {
    petSize.is.map(_.toString)
  }
  
  def render = {
    def sortProductSize(size1: String, size2: String) = {
      val size1Int = size1.replaceAll("[^\\d.]", "")
    }
    val petProductName = petProduct.is.map(_.name.toString)

     val products = petProductName.map { name => 
       Product.findAll(By(Product.name, name))
     }.openOr(Nil)
      
    val productSizes = products.map(_.size.toString)
   
    def findMinRangeInt(size: String) = { 
      val split = size.split("[-+]").headOption
      split.map(_.toInt).getOrElse(0)
    }

    val sortedSizes: Array[String] = productSizes.sortWith(findMinRangeInt(_) < findMinRangeInt(_)).toArray

    def chooseSize(size: String) = {

      val productWithSize = products.filter(_.size.toString == size).headOption

      productWithSize.map { product => 
        petSize(Full(product.size.get))
        petProduct(Full(product))
      }

      S.redirectTo(PetDetails.menu.loc.calcDefaultHref)
    }

    ".small .weight-number *" #> sortedSizes(0) &
    ".small #small-dog" #> SHtml.submit("Select", () => chooseSize(sortedSizes(0))) &
    ".medium .weight-number *" #> sortedSizes(1) &
    ".medium #medium-dog" #> SHtml.submit("Select", () => chooseSize(sortedSizes(1))) &
    ".large .weight-number *" #> sortedSizes(2) &
    ".large #large-dog" #> SHtml.submit("Select", () => chooseSize(sortedSizes(2))) &
    ".xlarge .weight-number *" #> sortedSizes(3) &
    ".xlarge #xlarge-dog" #> SHtml.submit("Select", () => chooseSize(sortedSizes(3)))
  }
}
