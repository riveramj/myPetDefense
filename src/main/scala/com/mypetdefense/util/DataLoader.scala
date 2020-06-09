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
    if (Product.findAll().isEmpty) {
      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatAllSize,
        sizeName = "All Sizes",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "adventure/Adventure-Plus-medium-cat.jpg",
        weight = 0.8,
        sku = "100011"
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large",
        imageName = "adventure/Adventure-Plus-large-cat.jpg",
        weight = 0.8,
        sku = "100012"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )

      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallAdv,
        sizeName = "Small",
        imageName = "adventure/Adventure-Plus-small-dog.jpg",
        weight = 0.8,
        sku = "100013"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumAdv,
        sizeName = "Medium",
        imageName = "adventure/Adventure-Plus-medium-dog.jpg",
        weight = 0.8,
        sku = "100014"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeAdv,
        sizeName = "Large",
        imageName = "adventure/Adventure-Plus-large-dog.jpg",
        weight = 0.8,
        sku = "100015"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeAdv,
        sizeName = "X-Large",
        imageName = "adventure/Adventure-Plus-xlarge-dog.jpg",
        weight = 0.8,
        sku = "100016"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small",
        imageName = "zoguard/ZoGuard-Plus-small-dog.jpg",
        weight = 0.8,
        sku = "100002"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium",
        imageName = "zoguard/ZoGuard-Plus-medium-dog.jpg",
        weight = 0.8,
        sku = "100003"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large",
        imageName = "zoguard/ZoGuard-Plus-large-dog.jpg",
        weight = 0.8,
        sku = "100004"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large",
        imageName = "zoguard/ZoGuard-Plus-xlarge-dog.jpg",
        weight = 0.8,
        sku = "100005"
      )

      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallShld,
        sizeName = "Small",
        imageName = "shieldtec/ShieldTec-Plus-small-dog.jpg",
        weight = 0.8,
        sku = "100007"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumShld,
        sizeName = "Medium",
        imageName = "shieldtec/ShieldTec-Plus-medium-dog.jpg",
        weight = 0.8,
        sku = "100008"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeShld,
        sizeName = "Large",
        imageName = "shieldtec/ShieldTec-Plus-large-dog.jpg",
        weight = 0.8,
        sku = "100009"
      )
      Product.createProduct(
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

  def updateProductSku = {
    val productsToUpdate = List(
      ("Adventure Plus for Cats", AnimalSize.CatMedium, "100011"),
      ("Adventure Plus for Cats", AnimalSize.CatLarge, "100012"),
      ("ZoGuard Plus for Cats", AnimalSize.CatSmall, "100011"),
      ("ZoGuard Plus for Cats", AnimalSize.CatMedium, "100011"),
      ("ZoGuard Plus for Cats", AnimalSize.CatLarge, "100011"),
      ("Adventure Plus for Dogs", AnimalSize.DogSmallAdv, "100013"),
      ("Adventure Plus for Dogs", AnimalSize.DogMediumAdv, "100014"),
      ("Adventure Plus for Dogs", AnimalSize.DogLargeAdv, "100015"),
      ("Adventure Plus for Dogs", AnimalSize.DogXLargeAdv, "100016"),
      ("ZoGuard Plus for Dogs", AnimalSize.DogSmallZo, "100002"),
      ("ZoGuard Plus for Dogs", AnimalSize.DogMediumZo, "100003"),
      ("ZoGuard Plus for Dogs", AnimalSize.DogLargeZo, "100004"),
      ("ZoGuard Plus for Dogs", AnimalSize.DogXLargeZo, "100005"),
      ("ShieldTec Plus for Dogs", AnimalSize.DogSmallShld, "100007"),
      ("ShieldTec Plus for Dogs", AnimalSize.DogMediumShld, "100008"),
      ("ShieldTec Plus for Dogs", AnimalSize.DogLargeShld, "100009"),
      ("ShieldTec Plus for Dogs", AnimalSize.DogXLargeShld, "100010"),
    )

    val productSku = Product.find(By(Product.sku, "100011"))

    if (productSku.isEmpty) {
      for {
        (productName, size, sku) <- productsToUpdate
        product <- Product.find(
          By(Product.name, productName),
          By(Product.size, size)
        )
      } yield {
        product.sku(sku).saveMe
      }
    }
  }

  def loadWelcomeInserts = {
    if (Insert.findAll().isEmpty) {
      Insert.createNewInsert("Welcome Brochure", "450006", .5)
      Insert.createNewInsert("TPP Registrations Welcome Insert", "450027", .0)
      Insert.createNewInsert("Petland Welcome Insert", "450028", .0)
    }
  }

  def addWeightToInsert = {
    val inserts = Insert.findAll()

    inserts map { insert =>
      if (insert.name.get.contains("Insert"))
        insert.weight(.1).saveMe
      else
        insert.weight(.4).saveMe
    }
  }

  def loadPetlandInsert = {
    if (Insert.find(By(Insert.name, "Petland Welcome Insert")).isEmpty) {
      Insert.createNewInsert("Petland Welcome Insert", "450028", .1)
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
        "rivera.mj@gmail.com",
        "password",
        "(404) 409-0724",
        None,
        None,
        mpdAgency,
        UserType.Admin
      )
    }
  }

  def updateParentNoPets = {
    val parents = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Active))

    parents.map { parent =>
      val pets = parent.activePets

      if (pets.size == 0)
        parent.getSubscription.map(_.status(Status.UserSuspended).saveMe)
    }
  }

  def createFriendsFamilyProducts = {
    val products = List(("511101","ZoGuard Plus for Cats (1.5 lbs and up) 3pk"),("511102","ZoGuard Plus for Dogs (5-22 lbs) 3pk"),("511103","ZoGuard Plus for Dogs (23-44 lbs) 3pk"),("511104","ZoGuard Plus for Dogs (45-88 lbs) 3pk"),("511105","ZoGuard Plus for Dogs (89-132 lbs) 3pk"),("511107","ShieldTec for Cats (1.5 lbs and up) 3pk"),("511112","ShieldTec Plus for Dogs (5-15 lbs) 4pk"),("511113","ShieldTec Plus for Dogs (16-33 lbs) 4pk"),("511114","ShieldTec Plus for Dogs (34-65 lbs) 4pk"),("511115","ShieldTec Plus for Dogs (66+ lbs and up) 4pk"),("511125","Adventure Plus for Cats (5-9 lbs) 4pk"),("511126","Adventure Plus for Cats (9 lbs and up) 4pk"),("511127","Adventure Plus for Dogs (3-10 lbs) 4pk"),("511128","Adventure Plus for Dogs (11-20 lbs) 4pk"),("511129","Adventure Plus for Dogs (21-55 lbs) 4pk"),("511130","Adventure Plus for Dogs (55 lbs and up) 4pk"),("512001","Salvo Flea & Tick Collar for Dogs (Small) 2pk"),("512002","Salvo Flea & Tick Collar for Dogs (Large) 2pk"),("514002","ShieldTec Flea & Tick Pet Spray (16 fl. Oz.)"))

    if (FriendsFamilyProduct.findAll().isEmpty) {
      products.map { case (sku, description) =>
        FriendsFamilyProduct.createProduct(sku, description)
      }
    }
  }

  def createProductAccessKey = {
    val possibleActiveParentsWithoutKey = User.findAll(
      By(User.userType, UserType.Parent),
      By(User.status, Status.Active),
      NullRef(User.productSalesKey)
    )

    val activeParentsWithoutKey = possibleActiveParentsWithoutKey.filter { parent =>
      val subscription = parent.getSubscription
      val subscriptionIsActive_? = subscription.map(_.status.get == Status.Active).getOrElse(false)
      val userHasPets_? = parent.activePets.size > 0

      (subscriptionIsActive_? && userHasPets_?)
    }

    activeParentsWithoutKey.map { parent =>
      val productKey = KeyService.createProductSalesKey

      parent.productSalesKey(productKey).saveMe
    }
  }

  def updateShipmentShipStationId = {
    val allShipments = Shipment.findAll()

    allShipments.map(_.shipStationOrderId(-1).saveMe)
  }

  def updateGeorgiaTaxRates = {
    val activeUsers = User.findAll(
      By(User.userType, UserType.Parent),
      NotBy(User.status, Status.Cancelled)
    )

    for {
      user <- activeUsers
      address <- user.shippingAddress
        if (address.state.get.toLowerCase == "ga")
      subscription <- user.getSubscription
    } yield {

      val stripeId = user.stripeId.get
      val subscriptionId = subscription.stripeSubscriptionId.get

      val (amount, rate) = TaxJarService.findTaxAmoutAndRate(
        address.city.get,
        address.state.get,
        address.zip.get,
        12.99
      )

      ParentService.updateTaxRate(stripeId, subscriptionId, rate, user.email.get)
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
    if (Treat.findAll().isEmpty) {
      Treat.createNewTreat("Duck Jerky Multivitamin & Immune Maintenance", 14.99, 8, "duck12345")
      Treat.createNewTreat("Lamb Jerky Digestive Health & Probiotic", 14.99, 8, "lamb12345")
      Treat.createNewTreat("Beef Jerky Hip & Joint Formula", 14.99, 8, "beef12345")
      //Treat.createNewTreat("Chicken Jerky Skin & Coat Formula", 14.99, 8, "chicken12345")
    }
  }

  def createFruitVeg = {
    Treat.createNewTreat("Mind Your Peas Natural Dog Treats", 12.99, 3, "fruit-small12345")
  }

  def replaceChickenTreat = {
    Treat.find(By(Treat.name, "Chicken Jerky Skin & Coat Formula")).map(_.delete_!)
    Treat.createNewTreat("Salmon Jerky Skin & Coat Formula", 14.99, 8, "salmon12345")
  }

  def createPackaging = {
    if (Packaging.findAll().isEmpty) {
      Packaging.createNewPackaging(
        "Bubble Mailer",
        0.3,
        "bubble1234",
        0,
        3
      )

      Packaging.createNewPackaging(
        "Small Box",
        2.5,
        "smallBox1234",
        0,
        5
      )

      Packaging.createNewPackaging(
        "Large Box",
        4.5,
        "largeBox1234",
        4,
        8
      )
    }
  }

  def addWeightToFleaTick = {
    Product.findAll().map(_.weight(0.8).saveMe)
  }

  def createOneSizeCat = {
    val oneSizeCat = Product.find(By(Product.size, AnimalSize.CatAllSize))
    if (oneSizeCat.isEmpty) {
      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatAllSize,
        sizeName = "All Sizes",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg",
        weight = 0.8,
        sku = "100001"
      )
    }
  }

  def removeFrontlineProduct = {
    def findProduct(name: String, size: AnimalSize.Value) = {
      Product.find(By(Product.name, name), By(Product.size, size))
    }

    val smallProduct = (findProduct("Frontline Plus for Dogs", AnimalSize.DogSmallZo), findProduct("ZoGuard Plus for Dogs", AnimalSize.DogSmallZo))
    val mediumProduct = (findProduct("Frontline Plus for Dogs", AnimalSize.DogMediumZo), findProduct("ZoGuard Plus for Dogs", AnimalSize.DogMediumZo))
    val largeProduct = (findProduct("Frontline Plus for Dogs", AnimalSize.DogLargeZo), findProduct("ZoGuard Plus for Dogs", AnimalSize.DogLargeZo))
    val xlargeproduct = (findProduct("Frontline Plus for Dogs", AnimalSize.DogXLargeZo), findProduct("ZoGuard Plus for Dogs", AnimalSize.DogXLargeZo))

    val productSizes = List(smallProduct, mediumProduct, largeProduct, xlargeproduct)

    for {
      productSize <- productSizes
      frontline <- productSize._1
    } yield {
      val pets = Pet.findAll(By(Pet.product, frontline))

      println(pets.map(_.petId))

      productSize._2.map { zoguard =>
        pets.map(_.product(zoguard).saveMe)
      }

      frontline.delete_!
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

  def renameVegTreat = {
    val fruitTreats = Treat.find(By(Treat.name, "Healthy Harvest Fruit and Veggie Mix"))

    fruitTreats.map(_.name("Mind Your Peas Natural Dog Treats").saveMe)
  }
}
