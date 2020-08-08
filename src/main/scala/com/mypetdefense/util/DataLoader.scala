package com.mypetdefense.util

import com.mypetdefense.model._
import com.mypetdefense.service._
import net.liftweb._
  import common._
  import util._

import net.liftweb.util.Helpers._
import net.liftweb.mapper._
import me.frmr.stripe.{Coupon => StripeCoupon, Subscription => _}
import dispatch._, Defaults._

object DataLoader extends Loggable {
  def loadProducts = {
    if (FleaTick.findAll().isEmpty) {
      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatAllSize,
        sizeName = "All Sizes",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      FleaTick.createFleaTick(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "adventure/Adventure-Plus-medium-cat.jpg",
        weight = 0.8,
        sku = "100011"
      )

      FleaTick.createFleaTick(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large",
        imageName = "adventure/Adventure-Plus-large-cat.jpg",
        weight = 0.8,
        sku = "100012"
      )

      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      FleaTick.createFleaTick(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallAdv,
        sizeName = "Small",
        imageName = "adventure/Adventure-Plus-small-dog.jpg",
        weight = 0.8,
        sku = "100013"
      )
      FleaTick.createFleaTick(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumAdv,
        sizeName = "Medium",
        imageName = "adventure/Adventure-Plus-medium-dog.jpg",
        weight = 0.8,
        sku = "100014"
      )
      FleaTick.createFleaTick(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeAdv,
        sizeName = "Large",
        imageName = "adventure/Adventure-Plus-large-dog.jpg",
        weight = 0.8,
        sku = "100015"
      )
      FleaTick.createFleaTick(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeAdv,
        sizeName = "X-Large",
        imageName = "adventure/Adventure-Plus-xlarge-dog.jpg",
        weight = 0.8,
        sku = "100016"
      )

      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small",
        imageName = "zoguard/ZoGuard-Plus-small-dog.jpg",
        weight = 0.8,
        sku = "100002"
      )
      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium",
        imageName = "zoguard/ZoGuard-Plus-medium-dog.jpg",
        weight = 0.8,
        sku = "100003"
      )
      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large",
        imageName = "zoguard/ZoGuard-Plus-large-dog.jpg",
        weight = 0.8,
        sku = "100004"
      )
      FleaTick.createFleaTick(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large",
        imageName = "zoguard/ZoGuard-Plus-xlarge-dog.jpg",
        weight = 0.8,
        sku = "100005"
      )

      FleaTick.createFleaTick(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallShld,
        sizeName = "Small",
        imageName = "shieldtec/ShieldTec-Plus-small-dog.jpg",
        weight = 0.8,
        sku = "100007"
      )
      FleaTick.createFleaTick(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumShld,
        sizeName = "Medium",
        imageName = "shieldtec/ShieldTec-Plus-medium-dog.jpg",
        weight = 0.8,
        sku = "100008"
      )
      FleaTick.createFleaTick(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeShld,
        sizeName = "Large",
        imageName = "shieldtec/ShieldTec-Plus-large-dog.jpg",
        weight = 0.8,
        sku = "100009"
      )
      FleaTick.createFleaTick(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeShld,
        sizeName = "X-Large",
        imageName = "shieldtec/ShieldTec-Plus-xlarge-dog.jpg",
        weight = 0.8,
        sku = "100010"
      )
    }
  }

  def loadAdmin = {
    val mpdAgency = {
      val possibleMpd = Agency.find(By(Agency.name, "My Pet Defense"))

      if (possibleMpd.isEmpty)
        Full(Agency.createNewAgency("My Pet Defense"))
      else
        possibleMpd
    }

    if (User.findAll(By(User.userType, UserType.Admin)).isEmpty) {
      User.createNewUser(
        "Mike",
        "Rivera",
        "",
        "riveramj@gmail.com",
        "password",
        "(404) 409-0724",
        None,
        None,
        mpdAgency,
        UserType.Admin
      )
    }
  }

  def resetUpcomingBillingCylces = {
    val upcomingSubscriptions = Subscription.findAll(
      BySql(
        "nextShipDate > CURRENT_DATE + interval '1 day' and nextShipDate < CURRENT_DATE + interval '3 day'",
        IHaveValidatedThisSQL("mike","2018-04-24")
      ),
      By(Subscription.status, Status.Active)
    )

    for {
      subscription <- upcomingSubscriptions
      user <- subscription.user.obj
    } yield {
      ParentService.updateNextShipDate(subscription, Full(user))
    }
  }

  def createProducts = {
    if (Product.hipAndJoint.isEmpty) {
      Product.createNewProduct("Hip & Joint Chews",  "hipJointChews")
      Product.createNewProduct("Calming Chews", "calmingChews")
      Product.createNewProduct("Multi-Vitamin Chews", "multiVitaminChews")
      Product.createNewProduct("Dental Powder", "dentalPowder")
    }
  }

  def createBoxedTag = {
    if (Tag.findAll().isEmpty) {
      Tag.createNewTag("Use Box")
    }
  }

  def createNewPetlandStores = {
    val petlandAgencies = Agency.findAll(By(Agency.petlandStore, true))
    
    val agents = petlandAgencies.map(_.members.toList).flatten

    agents.map(_.delete_!)
    
    petlandAgencies.map(_.delete_!)

    val petlandHQ = Full(Agency.createNewAgency(
      "Petland",
      AgencyType.Headquarters,
      Empty,
      "petlandhq",
      true
    ))

    Agency.createNewAgency(
      "Petland Carriage Place",
      AgencyType.Store,
      petlandHQ,
      "zplohcp",
      true
    )

    Agency.createNewAgency(
      "Petland Lewis Center",
      AgencyType.Store,
      petlandHQ,
      "zplohlc",
      true
    )

    Agency.createNewAgency(
      "Petland Kennesaw",
      AgencyType.Store,
      petlandHQ,
      "zplgak",
      true
    )

    Agency.createNewAgency(
      "Petland Mall of Georgia",
      AgencyType.Store,
      petlandHQ,
      "pmog",
      true
    )

    Agency.createNewAgency(
      "Petland Sarasota",
      AgencyType.Store,
      petlandHQ,
      "zplfls",
      true
    )

    Agency.createNewAgency(
      "Petland Summerville",
      AgencyType.Store,
      petlandHQ,
      "ps68",
      true
    )
  }

  def createPuppySpot = {
    val puppySpot = Agency.createNewAgency(
      "PuppySpot",
      AgencyType.Store,
      Empty,
      "pupspot",
      false
    )

    val tppAgency = Agency.find(By(Agency.name, "TPP"))

    val tppCustomers = tppAgency.map(_.customers.toList).openOr(Nil)

    tppCustomers.map(_.referer(puppySpot).saveMe)
  }

  def calculateTax = {
    val parents = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))
    
    for {
      parent <- parents
      address <- parent.shippingAddress
    } yield {
      if (address.state.get.toLowerCase == "ga" && parent.taxRate.get == 0D) { 
        parent.setTaxRate
      } else if (address.state.get.toLowerCase != "ga") {
        parent.taxRate(0D).saveMe
      }
    }
  }

  def createBasicExistingBoxes = {
    if (SubscriptionBox.findAll().isEmpty) {
      for {
        user <- User.findAll(By(User.userType, UserType.Parent), By(User.status,Status.Active))
        subscription <- Subscription.find(By(Subscription.user, user)).toList
        pet <- subscription.getPets
        fleaTick <- FleaTick.find(By(FleaTick.size, pet.size.get), By(FleaTick.animalType, pet.animalType.get))
      } yield {
        val box = SubscriptionBox.createBasicBox(subscription, fleaTick, pet)
        pet.box(box).saveMe()
        user.subscription(subscription).saveMe()
      }
    }
  }

  def defaultSaleCoupons = {
    if(Coupon.find(By(Coupon.couponCode, "50off")).isEmpty) {
      val mpdAgency = Agency.find(By(Agency.name, "My Pet Defense"))
      CouponService.createCoupon("50off", mpdAgency, "1", "50", "0")
      CouponService.createCoupon("100off", mpdAgency, "1", "100", "0")
    }
  }
}
