package com.mypetdefense.util

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.mapper.By

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
    }
  }

  def loadUsers = {
    if (User.findAll(By(User.userType, UserType.Agent)).isEmpty) {
      User.createNewUser(
        "John",
        "smith",
        "",
        "rivera.mj+agent@gmail.com",
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
        "(404) 409-0724",
        None,
        UserType.Parent
      )
    }
  }
}
