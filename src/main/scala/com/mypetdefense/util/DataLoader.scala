package com.mypetdefense.util

import com.mypetdefense.model.Agency.getHQFor
import com.mypetdefense.model._
import com.mypetdefense.service._
import net.liftweb.common.Box.tryo
import net.liftweb.common._
import net.liftweb.mapper._

import java.text.SimpleDateFormat

object DataLoader extends Loggable {
  def loadFleaTick: Any = {
    if(FleaTick.findAll().isEmpty) {
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

  def testLoadPrices = {
    if(Price.findAll().isEmpty) {
      val allFleaTick = FleaTick.findAll(By(FleaTick.animalType, AnimalType.Dog))
        .filter(_.isZoGuard_?)
      val tppPrice = 12.99D
      val smallHwBox = 24.99
      val mdXLHwBox = 27.99

      for {
        fleaTick <- allFleaTick
        code <- List(Price.defaultPriceCode, Price.currentTppPriceCode)
      } yield {
        Price.createPrice(
          price = tppPrice,
          code = code,
          fleaTick = Full(fleaTick),
          stripeName = "",
          boxType = Full(BoxType.basic)
        )
      }

      for {
      fleaTick <- allFleaTick.filterNot(_.size.get == AnimalSize.DogSmallZo)
      code <- List(Price.defaultPriceCode, Price.currentTppPriceCode)
      } yield {
        Price.createPrice(
          price = smallHwBox,
          code = code,
          fleaTick = Full(fleaTick),
          stripeName = "",
          boxType = Full(BoxType.healthAndWellness)
        )
      }

      for {
        fleaTick <- allFleaTick.filter(_.size.get == AnimalSize.DogSmallZo)
        code <- List(Price.defaultPriceCode, Price.currentTppPriceCode)
      } yield {
        Price.createPrice(
          price = mdXLHwBox,
          code = code,
          fleaTick = Full(fleaTick),
          stripeName = "",
          boxType = Full(BoxType.healthAndWellness)
        )
      }
    }
  }

  def loadAdmin: Any = {
    val mpdAgency = {
      val possibleMpd = Agency.mpdAgency

      if(possibleMpd.isEmpty)
        Full(Agency.createNewAgency("My Pet Defense"))
      else
        possibleMpd
    }

    if(User.findAll(By(User.userType, UserType.Admin)).isEmpty) {
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
        UserType.Admin,
        ""
      )
    }
  }

  def resetUpcomingBillingCycles: List[Serializable] = {
    val upcomingSubscriptions = Subscription.findAll(
      BySql(
        "nextShipDate > CURRENT_DATE + interval '1 day' and nextShipDate < CURRENT_DATE + interval '3 day'",
        IHaveValidatedThisSQL("mike", "2018-04-24")
      ),
      By(Subscription.status, Status.Active)
    )

    for {
      subscription <- upcomingSubscriptions
      _ <- subscription.user.obj
    } yield {
      ParentService.updateNextShipDate(subscription)
    }
  }

  def createProducts: Any = {
    for {
      quantity <- List(10, 30)
        if Product.findAll(By(Product.quantity, quantity)).isEmpty
    } yield {
      Product.createNewProduct("Hip & Joint Chews For Dogs", s"hipJointChews-$quantity", quantity, AnimalType.Dog, true)
      Product.createNewProduct("Calming Chews For Dogs", s"calmingChews-$quantity", quantity, AnimalType.Dog, true)
      Product.createNewProduct("Multi-Vitamin Chews For Dogs", s"multiVitaminChews-$quantity", quantity, AnimalType.Dog, true)
      Product.createNewProduct("Skin and Coat Chews For Dogs", s"skinAndCoatChews-$quantity", quantity, AnimalType.Dog, true)
      Product.createNewProduct("Probiotic Chews For Dogs", s"skinAndCoatChews-$quantity", quantity, AnimalType.Dog, true)
    }
  }

  def createMissingProbiotic =
    Product.createNewProduct("Probiotic Chews For Dogs", s"skinAndCoatChews-30", 30, AnimalType.Dog, true)

  def createBoxedTag: Any = {
    if(Tag.findAll().isEmpty) {
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

    val tppAgency = Agency.tppAgency

    val tppCustomers = tppAgency.map(_.customers.toList).openOr(Nil)

    tppCustomers.map(_.referer(puppySpot).saveMe)
  }

  def calculateTax: List[Any] = {
    val parents = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))

    for {
      parent <- parents
      address <- parent.shippingAddress
    } yield {
      if(address.state.get.toLowerCase == "ga" && parent.taxRate.get == 0d) {
        parent.setTaxRate
      } else if(address.state.get.toLowerCase != "ga") {
        parent.taxRate(0d).saveMe
      }
    }
  }

  def createBasicExistingBoxes: Any = {
    if(SubscriptionBox.findAll().isEmpty) {
      for {
        user <- User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))
        subscription <- Subscription.find(By(Subscription.user, user)).toList
        pet <- subscription.getPets
      } yield {
        val box = SubscriptionBox.createNewBox(subscription, pet)
        pet.box(box).saveMe()
        user.subscription(subscription).saveMe()
      }
    }
  }

  def connectCancelledUsersToSubscription() {
    for {
      user <- User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Cancelled))
      subscription <- Subscription.find(By(Subscription.user, user)).toList
    } yield {
      user.subscription(subscription).saveMe()
    }
  }

  def defaultSaleCoupons: Any = {
    if(Coupon.find(By(Coupon.couponCode, "50off")).isEmpty) {
      val mpdAgency = Agency.mpdAgency
      CouponService.createCoupon("50off", mpdAgency, "1", "50", "0")
      CouponService.createCoupon("100off", mpdAgency, "1", "100", "0")
    }
  }

  def upgradeInsert: Any =
    if(Insert.tryUpgrade.isEmpty) {
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
      if(box.subscriptionItems.toList.nonEmpty)
        subscription.isUpgraded(true).saveMe()
    }
  }

  def findSeptEarlyShipments() {
    val dateFormatter = new SimpleDateFormat("M/d/y")
    val startFree = dateFormatter.parse("8/7/2020")
    val endFree = dateFormatter.parse("9/6/2020")
    val freeMonth = dateFormatter.parse("8/1/2020")

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
    val startFree = dateFormatter.parse("9/3/2020")
    val endFree = dateFormatter.parse("9/10/2020")

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
    val startDate = dateFormatter.parse("7/1/2020")

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
    val startDate = dateFormatter.parse("7/1/2020")

    for {
      badUser <- User.findAll(NullRef(User.subscription), By_>(User.createdAt, startDate))
    } {
      val dupUsers = User.findAll(By(User.email, badUser.email.get))

      if(dupUsers.size > 1) {
        val possibleBadUserSub = Subscription.find(By(Subscription.user, badUser)).toList
        val possibleBadUserAddress = Address.findAll(By(Address.user, badUser))
        val pets = Pet.findAll(By(Pet.user, badUser))

        val badUserInfo = List(possibleBadUserSub, possibleBadUserAddress, pets).flatten

        if(badUserInfo.isEmpty)
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
      val updatedCat = cat.size(catFleaTick.size.get).saveMe
      val box = SubscriptionBox.createNewBox(subscription, updatedCat, false)
      updatedCat.box(box).saveMe()
    }

  def createMissingDogBoxes(): Unit =
    for {
      dog <- Pet.findAll(By(Pet.animalType, AnimalType.Dog), NullRef(Pet.box))
      user <- dog.user.obj.toList
      subscription <- user.subscription.obj.toList
    } {
      val box = SubscriptionBox.createNewBox(subscription, dog)
      dog.box(box).saveMe()

      if(subscription.isUpgraded.get) {
        SubscriptionItem.createFirstBox(box)
      }
    }

  def cancellationDataSync(): Unit = {
    def cancelPets(pets: List[Pet]): Unit = {
      pets.filter(_.status.get != Status.Cancelled).foreach(_.status(Status.Cancelled).saveMe())
    }

    for {
      subscription <- Subscription.findAll(By(Subscription.status, Status.Cancelled))
      user <- subscription.user.obj.toList
      pets = user.pets.toList
    } {
      if(user.status.get != Status.Cancelled)
        user.status(Status.Cancelled).saveMe()

      cancelPets(pets)
    }

    for {
      user <- User.findAll(By(User.status, Status.Cancelled))
      subscription <- user.subscription.obj.toList
      pets = user.pets.toList
    } {
      if(subscription.status.get != Status.Cancelled)
        subscription.status(Status.Cancelled).saveMe()

      cancelPets(pets)
    }
  }

  def subscriptionBoxCheck(): Unit = {
    if (Product.dentalPowderSmallForDogs.isEmpty) {
      Product.createNewProduct("Dental Powder For Dogs (Small)", "dentalPowderSmallDog", 0, AnimalType.Dog, false)
      Product.createNewProduct("Dental Powder For Dogs (Large)", "dentalPowderLargeDog", 0, AnimalType.Dog, false)
    }

    val products = List(
      Product.skinAndCoatForDogs(10),
      Product.multiVitaminForDogs(10),
      Product.probioticForDogs(10),
      Product.dentalPowderForDogs
    ).flatten

    for {
      box <- SubscriptionBox.findAll(
        By(SubscriptionBox.userModified, true),
        By(SubscriptionBox.animalType, AnimalType.Dog),
        By(SubscriptionBox.boxType, BoxType.healthAndWellness)
      )
      subscriptionItems = box.subscriptionItems.toList.flatMap(_.product.obj)
    } yield {
      if(subscriptionItems.forall(products.contains))
        box.userModified(false).save()
    }
  }

  def upgradeSubscriptionBoxDetails(): Unit = {
    for {
      box <- SubscriptionBox.findAll()
      pet <- box.pet.obj
      products = box.subscriptionItems.toList
    } yield {
      if(products.nonEmpty)
        box
          .animalType(AnimalType.Cat)
          .animalType(pet.animalType.get)
          .boxType(BoxType.healthAndWellness)
          .saveMe()
      else
        box
          .animalType(AnimalType.Cat)
          .animalType(pet.animalType.get)
          .boxType(BoxType.healthAndWellness)
          .boxType(BoxType.basic)
          .userModified(false)
          .saveMe()
    }
  }

  def rebuildSubscriptionUpgrades(): Unit = {
    val upgradedBasicSubs = Subscription.findAll(
      NotBy(Subscription.priceCode, "2.0-launch")
    )

    val alreadyIncluded =
      SubscriptionUpgrade.findAll().map(_.subscription.get).toSet

    val subsToProcess =
      upgradedBasicSubs.filterNot(s => alreadyIncluded(s.id.get))

    for {
      sub <- subsToProcess
      user <- sub.user.toList
      referrer <- user.referer.obj.map(getHQFor)
      (upgradedShipment, shipmentNumber) <- sub.shipments
        .sortBy(_.createdAt.get)
        .zipWithIndex
        .find { case (shipment, _) =>
          shipment.shipmentLineItems.exists(!_.product.isEmpty) &&
          !shipment.freeUpgradeSample.get &&
          tryo(shipment.amountPaid.get.toDouble).map(_ > 12.99).openOr(false)
        }
    } yield {
      val shipmentCount = shipmentNumber + 1
      val upgradeDate = upgradedShipment.dateProcessed.get

      SubscriptionUpgrade.create
        .subscription(sub)
        .user(user)
        .referrer(referrer)
        .shipmentCountAtUpgrade(shipmentCount)
        .upgradeDate(upgradeDate)
        .saveMe
    }
  }

  def cancelBoxesForCancelledPets(): Seq[SubscriptionBox] = {
    for {
      pet <- Pet.findAll(By(Pet.status, Status.Cancelled))
      box <- pet.box.obj
    } yield {
      box.status(Status.Cancelled).saveMe()
    }
  }

  def updateChewCounts() = {
    for {
      supplement <- Product.findAll(
        Like(Product.name, "%Chews"),
        NullRef(Product.quantity)
      )
      name = supplement.name.get
    } yield {
      supplement
        .animalType(AnimalType.Cat)
        .animalType(AnimalType.Dog)
        .quantity(10)
        .sku(s"${supplement.name.get}-10")
        .name(s"${supplement.name.get} for Dogs")
        .saveMe()
    }

    Product.find(By(Product.sku, "dentalPowderSmall")).map { dental =>
      dental
        .sku("dentalPowderSmallDog")
        .name("Dental Powder For Dogs (Small)")
        .quantity(1)
        .quantity(0)
        .animalType(AnimalType.Cat)
        .animalType(AnimalType.Dog)
        .saveMe()
    }

    Product.find(By(Product.sku, "dentalPowderLarge")).map { dental =>
      dental
        .sku("dentalPowderLargeDog")
        .name("Dental Powder For Dogs (Large)")
        .quantity(1)
        .quantity(0)
        .animalType(AnimalType.Cat)
        .animalType(AnimalType.Dog)
        .saveMe()
    }
  }

  def migrateTo30DaySupply() = {
    val monthMultiVitamin = Product.multiVitaminForDogs(30).toList

    for {
      multiVitamin <- monthMultiVitamin
      box <- SubscriptionBox.findAll(
        By(SubscriptionBox.userModified, false),
        NullRef(SubscriptionBox.monthSupply)
      )
    } yield {
      box.monthSupply(true).saveMe()
      val boxItem = box.subscriptionItems.toList

      boxItem.foreach { item =>
        val possibleSupplement = item.product.obj
        if (possibleSupplement.map(_.isSupplement.get).openOrThrowException("Missing Supplement Check"))
          item.delete_!
      }

      SubscriptionItem.createSubscriptionItem(multiVitamin, box)
    }

    for {
      box <- SubscriptionBox.findAll(
        NullRef(SubscriptionBox.monthSupply)
      )
    } yield {
      box.monthSupply(false).saveMe()
    }
  }

  def createEmailReports(): List[EmailReport] = {
    if(EmailReport.findAll().isEmpty) {
      val emailReports = List(
        ("Daily TPP Agent Sales Report", "Daily email that shows sales by agent an store by day", ReportType.DailyTPPAgentSalesReportEmail),
        ("Daily MPD report", "Daily email that shows shipment statistics from previous day", ReportType.DailyInternalReportEmail),
        ("New Sale Email", "Instant email for every MPD.com sale.", ReportType.NewSaleEmail),
        ("New Upgrade Email", "Instant email for every upgrade to the H&W box.", ReportType.UpgradeSubscriptionEmail),
        ("Monthly TPP Agent Report", "Monthly email that shows sales by agent and store for the month", ReportType.MonthlyTPPAgentSalesReportEmail)
      )

      for {
        report <- emailReports
      } yield {
        EmailReport.createNewEmailReport(report._1, report._2, report._3)
      }
    } else Nil
  }

  def addMonthlyTPPAgentReport() = {
    if(EmailReport.find(By(EmailReport.reportType, ReportType.MonthlyTPPAgentSalesReportEmail)).isEmpty)
      EmailReport.createNewEmailReport(
        "Monthly TPP Agent Report",
        "Monthly email that shows sales by agent and store for the month",
        ReportType.MonthlyTPPAgentSalesReportEmail
      )
  }

  def markSupplements: Seq[Product] = {
    Product.findAll(NotLike(Product.name, "%Dental Powder%")).map(_.isSupplement(true).saveMe())
    Product.findAll(Like(Product.name, "%Dental Powder%")).map(_.isSupplement(false).saveMe())
  }

  def createMandrillTemplates =
    if (MandrillTemplate.findAll().isEmpty)
      createResetPasswordEmailTemplate

  def createResetPasswordEmailTemplate =
    MandrillTemplate.createMandrillTemplate("reset password temp", "reset password", EmailType.PasswordReset)

  def checkUpgradesAgainstBoxes =
    for {
      subscription <- Subscription.findAll(
        By(Subscription.isUpgraded, false),
        By(Subscription.status, Status.Cancelled),
      )
      boxes = subscription.subscriptionBoxes
        .filter(_.boxType == BoxType.healthAndWellness)
    } yield {
      if (boxes.nonEmpty)
        subscription.isUpgraded(true).saveMe()
    }

  def createCancelledSubForUser = {
    for {
    user <- User.findAll(
      By(User.userType, UserType.Parent),
      NullRef(User.subscription)
    )
    } yield {
      val (priceCode, isUpgraded) = if(user.salesAgentId.get.isEmpty)
        (Price.defaultPriceCode, true)
      else
        (Price.currentTppPriceCode, false)

      val subscription =
        Subscription.createNewSubscription(
          Full(user),
          "",
          user.createdAt.get,
          user.createdAt.get,
          priceCode,
          isUpgraded
        )
        .status(Status.Cancelled)
        .cancellationDate(user.createdAt.get)
        .saveMe()

      user.subscription(subscription).saveMe()
      user.cancel
    }
  }

  def dentalProductCleanup = {
    val smallDogDental = Product.dentalPowderSmallForDogs
    val largeDogDental = Product.dentalPowderLargeForDogs
    val smallLargeDental = List(largeDogDental, smallDogDental)
    val smallSizes = List(AnimalSize.DogSmallZo, AnimalSize.DogSmallShld, AnimalSize.DogSmallAdv)

    for {
      box <- SubscriptionBox.findAll(By(SubscriptionBox.status, Status.Active))
      items = box.subscriptionItems.toList
        if items.size > 4
    } yield {
      items.filter(_.product.obj == Product.dentalPowderForDogs).map(_.delete_!)
    }
  }
}
