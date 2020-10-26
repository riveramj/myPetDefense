package com.mypetdefense.util

import java.text.SimpleDateFormat

import com.mypetdefense.model._
import com.mypetdefense.service._
import me.frmr.stripe.{Subscription => _}
import net.liftweb.common._
import net.liftweb.mapper._

object DataLoader extends Loggable {
  def loadProducts: Any = {
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

  def loadAdmin: Any = {
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

  def resetUpcomingBillingCylces: List[Serializable] = {
    val upcomingSubscriptions = Subscription.findAll(
      BySql(
        "nextShipDate > CURRENT_DATE + interval '1 day' and nextShipDate < CURRENT_DATE + interval '3 day'",
        IHaveValidatedThisSQL("mike", "2018-04-24")
      ),
      By(Subscription.status, Status.Active)
    )

    for {
      subscription <- upcomingSubscriptions
      user         <- subscription.user.obj
    } yield {
      ParentService.updateNextShipDate(subscription, Full(user))
    }
  }

  def createProducts: Any = {
    if (Product.hipAndJoint.isEmpty) {
      Product.createNewProduct("Hip & Joint Chews", "hipJointChews")
      Product.createNewProduct("Calming Chews", "calmingChews")
      Product.createNewProduct("Multi-Vitamin Chews", "multiVitaminChews")
      Product.createNewProduct("Dental Powder", "dentalPowder")
    }

    if (Product.skinAndCoat.isEmpty) {
      Product.createNewProduct("Skin and Coat Chews", "skinAndCoatChews")
      Product.createNewProduct("Probiotic Chews", "probioticChews")
    }
  }

  def createBoxedTag: Any = {
    if (Tag.findAll().isEmpty) {
      Tag.createNewTag("Use Box")
    }
  }

  def createNewPetlandStores: Agency = {
    val petlandAgencies = Agency.findAll(By(Agency.petlandStore, true))

    val agents = petlandAgencies.flatMap(_.members.toList)

    agents.map(_.delete_!)

    petlandAgencies.map(_.delete_!)

    val petlandHQ = Full(
      Agency.createNewAgency(
        "Petland",
        AgencyType.Headquarters,
        Empty,
        "petlandhq",
        petlandStore = true
      )
    )

    Agency.createNewAgency(
      "Petland Carriage Place",
      AgencyType.Store,
      petlandHQ,
      "zplohcp",
      petlandStore = true
    )

    Agency.createNewAgency(
      "Petland Lewis Center",
      AgencyType.Store,
      petlandHQ,
      "zplohlc",
      petlandStore = true
    )

    Agency.createNewAgency(
      "Petland Kennesaw",
      AgencyType.Store,
      petlandHQ,
      "zplgak",
      petlandStore = true
    )

    Agency.createNewAgency(
      "Petland Mall of Georgia",
      AgencyType.Store,
      petlandHQ,
      "pmog",
      petlandStore = true
    )

    Agency.createNewAgency(
      "Petland Sarasota",
      AgencyType.Store,
      petlandHQ,
      "zplfls",
      petlandStore = true
    )

    Agency.createNewAgency(
      "Petland Summerville",
      AgencyType.Store,
      petlandHQ,
      "ps68",
      petlandStore = true
    )
  }

  def createPuppySpot: List[User] = {
    val puppySpot = Agency.createNewAgency("PuppySpot", AgencyType.Store, Empty, "pupspot")

    val tppAgency = Agency.find(By(Agency.name, "TPP"))

    val tppCustomers = tppAgency.map(_.customers.toList).openOr(Nil)

    tppCustomers.map(_.referer(puppySpot).saveMe)
  }

  def calculateTax: List[Any] = {
    val parents = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))

    for {
      parent  <- parents
      address <- parent.shippingAddress
    } yield {
      if (address.state.get.toLowerCase == "ga" && parent.taxRate.get == 0d) {
        parent.setTaxRate
      } else if (address.state.get.toLowerCase != "ga") {
        parent.taxRate(0d).saveMe
      }
    }
  }

  def createBasicExistingBoxes: Any = {
    if (SubscriptionBox.findAll().isEmpty) {
      for {
        user         <- User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))
        subscription <- Subscription.find(By(Subscription.user, user)).toList
        pet          <- subscription.getPets
        fleaTick <- FleaTick.find(
                     By(FleaTick.size, pet.size.get),
                     By(FleaTick.animalType, pet.animalType.get)
                   )
      } yield {
        val box = SubscriptionBox.createBasicBox(subscription, fleaTick, pet)
        pet.box(box).saveMe()
        user.subscription(subscription).saveMe()
      }
    }
  }

  def connectCancelledUsersToSubscription() {
    for {
      user         <- User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Cancelled))
      subscription <- Subscription.find(By(Subscription.user, user)).toList
    } yield {
      user.subscription(subscription).saveMe()
    }
  }

  def defaultSaleCoupons: Any = {
    if (Coupon.find(By(Coupon.couponCode, "50off")).isEmpty) {
      val mpdAgency = Agency.find(By(Agency.name, "My Pet Defense"))
      CouponService.createCoupon("50off", mpdAgency, "1", "50", "0")
      CouponService.createCoupon("100off", mpdAgency, "1", "100", "0")
    }
  }

  def upgradeInsert: Any =
    if (Insert.tryUpgrade.isEmpty) {
      Insert.createNewInsert(
        "Try Upgraded Box",
        "upgrade-insert",
        0.1
      )

      Insert.createNewInsert(
        "Summer Product Brochure 2020",
        "summer-brochure-2020",
        0.1
      )
    }

  def markUpgradedSubscriptions() {
    for {
      subscription <- Subscription.findAll()
      if !subscription.isUpgraded.get
      box <- subscription.subscriptionBoxes.headOption
    } yield {
      if (box.subscriptionItems.toList.nonEmpty)
        subscription.isUpgraded(true).saveMe()
    }
  }

  def findSeptEarlyShipments() {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startFree     = dateFormatter.parse("8/7/2020")
    val endFree       = dateFormatter.parse("9/6/2020")
    val freeMonth     = dateFormatter.parse("8/1/2020")

    for {
      shipment <- Shipment.findAll(
                   By_>(Shipment.dateShipped, startFree),
                   By_<(Shipment.dateShipped, endFree)
                 )
      subscription <- shipment.subscription.obj
      if subscription.freeUpgradeSampleDate.get == null
    } yield {
      subscription.freeUpgradeSampleDate(freeMonth).saveMe
    }
  }

  def clearRecentShipments() {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startFree     = dateFormatter.parse("9/3/2020")
    val endFree       = dateFormatter.parse("9/10/2020")

    for {
      shipment <- Shipment.findAll(
                   By_>(Shipment.dateProcessed, startFree),
                   By_<(Shipment.dateProcessed, endFree)
                 )
      if shipment.shipmentStatus.get == ShipmentStatus.LabelCreated
    } yield {
      shipment.shipStationOrderId(0).shipmentStatus(ShipmentStatus.Paid).saveMe()
    }
  }

  def connectBoxToPet() {
    for {
      pet <- Pet.findAll(NullRef(Pet.box))
      box <- SubscriptionBox.find(By(SubscriptionBox.pet, pet.id.get))
    } {
      pet.box(box).saveMe()
    }
  }

  def dataCleanUp() {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startDate     = dateFormatter.parse("7/1/2020")

    for {
      user <- User.findAll(NullRef(User.subscription), By_>(User.createdAt, startDate))
      subscription <- Subscription.find(By(Subscription.user, user))
    } {
      user.subscription(subscription).saveMe()
    }

    for {
      pet <- Pet.findAll(NullRef(Pet.box), By_>(Pet.createdAt, startDate))
      box <- SubscriptionBox.find(By(SubscriptionBox.pet, pet))
    } {
      pet.box(box).saveMe()
    }
  }

  def removeDupUsers() {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startDate     = dateFormatter.parse("7/1/2020")

    for {
      badUser <- User.findAll(NullRef(User.subscription), By_>(User.createdAt, startDate))
    } {
      val dupUsers = User.findAll(By(User.email, badUser.email.get))

      if (dupUsers.size > 1) {
        val possibleBadUserSub = Subscription.find(By(Subscription.user, badUser)).toList
        val possibleBadUserAddress = Address.findAll(By(Address.user, badUser))
        val pets = Pet.findAll(By(Pet.user, badUser))

        val badUserInfo = List(possibleBadUserSub, possibleBadUserAddress, pets).flatten

        if (badUserInfo.isEmpty)
          badUser.delete_!
      }
    }
  }

  def createMissingCatBoxes(): Unit =
    for {
      cat <- Pet.findAll(By(Pet.animalType, AnimalType.Cat), NullRef(Pet.box))
      user <- cat.user.obj.toList
      subscription <- user.subscription.obj.toList
      catFleaTick <- FleaTick.zoGuardCat.toList
    } {
      val box = SubscriptionBox.createBasicBox(subscription, catFleaTick, cat)
      cat.box(box).saveMe()
    }
}
