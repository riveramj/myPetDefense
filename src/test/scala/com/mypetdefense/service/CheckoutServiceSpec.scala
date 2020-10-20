package com.mypetdefense.service

import com.mypetdefense.generator.Generator
import com.mypetdefense.helpers.BootUtil
import com.mypetdefense.helpers.GeneralDbUtils.clearTables
import com.mypetdefense.model.{Address, Pet, Price, Status, Subscription, User}
import com.mypetdefense.snippet.signup.NewUserAddress
import net.liftweb.common.{Box, Empty}
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CheckoutServiceSpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with ScalaCheckPropertyChecks {

  override def beforeAll() {
    BootUtil.bootForTests()
  }

  override def afterAll(): Unit = {
    clearTables()
  }

  override def afterEach(): Unit = {
    clearTables()
  }

  private def compareAddressWithExpected(
      maybeAddress: Option[Address],
      expectedAddress: NewUserAddress
  ): Assertion = {
    maybeAddress.map(_.street1.get.toLowerCase) shouldBe Some(expectedAddress.street1.toLowerCase)
    maybeAddress.map(_.street2.get.toLowerCase) shouldBe Some(expectedAddress.street2.toLowerCase)
    maybeAddress.map(_.city.get.toLowerCase) shouldBe Some(expectedAddress.city.toLowerCase)
    maybeAddress.map(_.state.get.toLowerCase) shouldBe Some(expectedAddress.state.toLowerCase)
    maybeAddress.map(_.zip.get.toLowerCase) shouldBe Some(expectedAddress.zip.toLowerCase)
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
    sub.map(_.status.get) shouldBe Some(Status.Active)
    pets.size shouldBe petsToCreate.size
    compareAddressWithExpected(maybeAddress, generatedUserData.address)
  }

}
