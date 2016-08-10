package com.mypetdefense.util

import com.mypetdefense.model._
import net.liftweb._
  import common._
  import util._
import net.liftweb.mapper.By
import me.frmr.stripe.{Coupon => StripeCoupon, _}
import dispatch._, Defaults._

object DataLoader extends Loggable {
  def loadProducts = {
    if (Product.findAll().isEmpty) {
      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge
      )

      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallAdv
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumAdv
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeAdv
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeAdv
      )

      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo
      )

      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallShld
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumShld
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeShld
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeShld
      )
    }
  }

  def loadUsers = {
    if (User.findAll(By(User.userType, UserType.Agent)).isEmpty) {
      User.createNewUser(
        "John",
        "smith",
        "",
        "rivera.mj+agent@gmail.com",
        "password",
        "(404) 409-0724",
        None,
        UserType.Agent
      )
    }

    if (Retailor.findAll().isEmpty) {
      val possibleAgent = User.find(By(User.email, "rivera.mj+agent@gmail.com"))

      possibleAgent.map { agent =>
        Retailor.createNewRetailor(
          "Big Pets",
          agent  
        )
      }
    }
    
    if (Lead.findAll().isEmpty) {
      val possibleRetailor = Retailor.find(By(Retailor.name, "Big Pets"))
      
      possibleRetailor.map { retailor =>
        Lead.createNewLead(
          "Jane",
          "Doe",
          "rivera.mj+lead@gmail.com",
          "(404) 409-0724",
          retailor
        )
      }
    }

    if (User.findAll(By(User.userType, UserType.Parent)).isEmpty) {
      User.createNewUser(
        "Jane",
        "Doe",
        "stripe1234",
        "rivera.mj@gmail.com",
        "password",
        "(404) 409-0724",
        None,
        UserType.Parent
      )
    }
  }

  def loadCoupons = {
    def populateCouponsLocally(coupons: List[StripeCoupon]) = {
      for {
        coupon <- coupons
      } yield {
        Coupon.createCoupon(coupon.id)
      }
    }

    if(Coupon.findAll().isEmpty) {
      val stripeSecretKey = Props.get("secret.key") openOr ""
      implicit val e = new StripeExecutor(stripeSecretKey)

      val allCoupons = StripeCoupon.list

      for (coupons <- allCoupons)
        populateCouponsLocally(coupons.map(_.data).openOr(Nil))
    }
  }
}
