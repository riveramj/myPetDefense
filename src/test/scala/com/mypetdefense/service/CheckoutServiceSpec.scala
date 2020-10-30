package com.mypetdefense.service

import com.mypetdefense.generator.Generator
import com.mypetdefense.helpers.DBTest
import com.mypetdefense.model.{Address, Pet, Price, Status, Subscription, User}
import com.mypetdefense.snippet.signup.NewUserAddress
import cats.syntax.option._
import net.liftweb.common.{Box, Empty}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CheckoutServiceSpec extends DBTest {

  private def compareAddressWithExpected(
      maybeAddress: Option[Address],
      expectedAddress: NewUserAddress
  ): Assertion = {
    maybeAddress.map(_.street1.get.toLowerCase) shouldBe expectedAddress.street1.toLowerCase.some
    maybeAddress.map(_.street2.get.toLowerCase) shouldBe expectedAddress.street2.toLowerCase.some
    maybeAddress.map(_.city.get.toLowerCase) shouldBe expectedAddress.city.toLowerCase.some
    maybeAddress.map(_.state.get.toLowerCase) shouldBe expectedAddress.state.toLowerCase.some
    maybeAddress.map(_.zip.get.toLowerCase) shouldBe expectedAddress.zip.toLowerCase.some
  }

  it should "create user, subscription, address and pet on setting up new user" in {
    val user: Box[User]   = Empty
    val petsToCreate      = Generator.simplePetsNonEmptyList()
    val generatedUserData = Generator.newUserData()
    val stripeCustomer    = Generator.stripeCustomer()

    val maybeCreatedUser = CheckoutService.newUserSetup(
      user,
      petsToCreate,
      Price.defaultPriceCode,
      generatedUserData,
      stripeCustomer
    )
    maybeCreatedUser.isDefined shouldBe true
    val createdUser  = maybeCreatedUser.toOption.get
    val sub          = Subscription.findAll().find(_.user.get == createdUser.id.get)
    val pets         = Pet.findAll().filter(_.user.get == createdUser.id.get)
    val maybeAddress = Address.findAll().find(_.user.get == createdUser.id.get)

    sub.isDefined shouldBe true
    sub.map(_.status.get) shouldBe Status.Active.some
    pets.size shouldBe petsToCreate.size
    compareAddressWithExpected(maybeAddress, generatedUserData.address)
  }

}
