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
        size = AnimalSize.DogSmallZo,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "Frontline Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large"
      )
    }

    if (Product.find(By(Product.name, "Frontline Plus for Cats")).isEmpty) {
      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small"
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium"
      )

      Product.createProduct(
        name = "Frontline Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Lage"
      )
    }

    if (Product.findAll().isEmpty) {
      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium"
      )

      Product.createProduct(
        name = "Adventure Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatSmall,
        sizeName = "Small"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatMedium,
        sizeName = "Medium"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Cats",
        animalType = AnimalType.Cat,
        size = AnimalSize.CatLarge,
        sizeName = "Large"
      )

      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallAdv,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumAdv,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeAdv,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "Adventure Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeAdv,
        sizeName = "X-Large"
      )

      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallZo,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumZo,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeZo,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "ZoGuard Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeZo,
        sizeName = "X-Large"
      )

      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogSmallShld,
        sizeName = "Small"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogMediumShld,
        sizeName = "Medium"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogLargeShld,
        sizeName = "Large"
      )
      Product.createProduct(
        name = "ShieldTec Plus for Dogs",
        animalType = AnimalType.Dog,
        size = AnimalSize.DogXLargeShld,
        sizeName = "X-Large"
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
