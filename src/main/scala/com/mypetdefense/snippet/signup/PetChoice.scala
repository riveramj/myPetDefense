package com.mypetdefense.snippet.signup

import com.mypetdefense.model.{AnimalType, Coupon}
import com.mypetdefense.service._
import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper.By
import net.liftweb.sitemap.Loc.EarlyResponse
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._

object PetChoice extends Loggable {
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Pet Choice") / "pet-choice" >>
    EarlyResponse(() => {
      val woofTraxOfferCode = S.param("oc")
      PetFlowChoices.woofTraxOfferCode(woofTraxOfferCode)
      PetFlowChoices.woofTraxUserId(S.param("ui"))

      val possibleCoupon: Box[Coupon] = woofTraxOfferCode.flatMap(oc => Coupon.find(By(Coupon.couponCode, oc)))
      PetFlowChoices.coupon(possibleCoupon)

      S.redirectTo(DogDetails.menu.loc.calcDefaultHref)
    })
}

class PetChoice extends Loggable {
  import PetFlowChoices._

  def chosenPet: CssSel = "*" #> {
    petChoice.is.map(_.toString)
  }

  def render: CssSel = {
    def dogFlow = {
      petChoice(Full(AnimalType.Dog))

      if (petId.is.isEmpty)
        petId(Full(generateLongId))

      S.redirectTo(DogDetails.menu.loc.calcDefaultHref)
    }

    def catFlow = {
      petChoice(Full(AnimalType.Cat))

      if (petId.is.isEmpty)
        petId(Full(generateLongId))

      S.redirectTo(PetChoice.menu.loc.calcDefaultHref)
    }

    "#dog" #> SHtml.submit("Select", dogFlow _) &
      "#cat" #> SHtml.submit("Select", catFlow _)
  }
}
