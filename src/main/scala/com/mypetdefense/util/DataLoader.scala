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
    if (Product.find(By(Product.name, "Frontline Plus for Dogs")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo
      )
    }

    if (Product.find(By(Product.name, "Frontline Plus for Cats")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge
      )
    }

    if (Product.findAll().isEmpty) {
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
        size = AnimalSize.CatSmall
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
}
