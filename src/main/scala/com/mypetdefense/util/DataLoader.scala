package com.mypetdefense.util

import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService
import net.liftweb._
  import common._
  import util._
  
import net.liftweb.util.Helpers._
import net.liftweb.mapper.By
import me.frmr.stripe.{Coupon => StripeCoupon, Subscription => _}
import dispatch._, Defaults._

object DataLoader extends Loggable {
  def loadProducts = {
    if (Product.findAll().isEmpty) {
      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "adventure/Adventure-Plus-medium-cat.jpg"
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large",
        imageName = "adventure/Adventure-Plus-large-cat.jpg"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large",
        imageName = "zoguard/ZoGuard-Plus-cat-2.jpg"
      )

      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallAdv,
        sizeName = "Small",
        imageName = "adventure/Adventure-Plus-small-dog.jpg"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumAdv,
        sizeName = "Medium",
        imageName = "adventure/Adventure-Plus-medium-dog.jpg"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeAdv,
        sizeName = "Large",
        imageName = "adventure/Adventure-Plus-large-dog.jpg"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeAdv,
        sizeName = "X-Large",
        imageName = "adventure/Adventure-Plus-xlarge-dog.jpg"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small",
        imageName = "zoguard/ZoGuard-Plus-small-dog.jpg"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium",
        imageName = "zoguard/ZoGuard-Plus-medium-dog.jpg"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large",
        imageName = "zoguard/ZoGuard-Plus-large-dog.jpg"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large",
        imageName = "zoguard/ZoGuard-Plus-xlarge-dog.jpg"
      )

      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallShld,
        sizeName = "Small",
        imageName = "shieldtec/ShieldTec-Plus-small-dog.jpg"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumShld,
        sizeName = "Medium",
        imageName = "shieldtec/ShieldTec-Plus-medium-dog.jpg"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeShld,
        sizeName = "Large",
        imageName = "shieldtec/ShieldTec-Plus-large-dog.jpg"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeShld,
        sizeName = "X-Large",
        imageName = "shieldtec/ShieldTec-Plus-xlarge-dog.jpg"
      )
    }

    if (Product.find(By(Product.name, "Frontline Plus for Dogs")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small",
        imageName = "frontline/Frontline-Plus-small-dog.jpg"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium",
        imageName = "frontline/Frontline-Plus-medium-dog.jpg"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large",
        imageName = "frontline/Frontline-Plus-large-dog.jpg"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large",
        imageName = "frontline/Frontline-Plus-xlarge-dog.jpg"
      )
    }

    if (Product.find(By(Product.name, "Frontline Plus for Cats")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small",
        imageName = "frontline/Frontline-Plus-cat.jpg"
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium",
        imageName = "frontline/Frontline-Plus-cat.jpg"
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Lage",
        imageName = "frontline/Frontline-Plus-cat.jpg"
      )
    }
  }

  def loadAdmin = {
    if (User.findAll(By(User.userType, UserType.Admin)).isEmpty) {
      User.createNewUser(
        "John",
        "smith",
        "",
        "rivera.mj@gmail.com",
        "password",
        "(404) 409-0724",
        None,
        None,
        None,
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
    val products = List(("511101","ZoGuard Plus for Cats (1.5 lbs and up) 3pk"),("511102","ZoGuard Plus for Dogs (4-22 lbs) 3pk"),("511103","ZoGuard Plus for Dogs (23-44 lbs) 3pk"),("511104","ZoGuard Plus for Dogs (45-88 lbs) 3pk"),("511105","ZoGuard Plus for Dogs (89-132 lbs) 3pk"),("511107","ShieldTec for Cats (1.5 lbs and up) 3pk"),("511112","ShieldTec Plus for Dogs (5-15 lbs) 4pk"),("511113","ShieldTec Plus for Dogs (16-33 lbs) 4pk"),("511114","ShieldTec Plus for Dogs (34-65 lbs) 4pk"),("511115","ShieldTec Plus for Dogs (66+ lbs and up) 4pk"),("511125","Adventure Plus for Cats (5-9 lbs) 4pk"),("511126","Adventure Plus for Cats (9 lbs and up) 4pk"),("511127","Adventure Plus for Dogs (3-10 lbs) 4pk"),("511128","Adventure Plus for Dogs (11-20 lbs) 4pk"),("511129","Adventure Plus for Dogs (21-55 lbs) 4pk"),("511130","Adventure Plus for Dogs (55 lbs and up) 4pk"),("512001","Salvo Flea & Tick Collar for Dogs (Small) 2pk"),("512002","Salvo Flea & Tick Collar for Dogs (Large) 2pk"),("514002","ShieldTec Flea & Tick Pet Spray (16 fl. Oz.)"))

    if (FriendsFamilyProduct.findAll().isEmpty) {
      products.map { case (sku, description) =>
        FriendsFamilyProduct.createProduct(sku, description)
      }
    }
  }

  def updateShipmentShipStationId = {
    val allShipments = Shipment.findAll()

    allShipments.map(_.shipStationOrderId(-1).saveMe)
  }
}
