package com.mypetdefense.util

import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.mapper.By

object DataLoader extends Loggable {
  def loadProducts = {
    if (Product.findAll().isEmpty) {
      Product.createProduct(
        "Urban Product",
        "For the urban-chic dog",
        5,
        AnimalType.Dog,
        AnimalSize.Medium
      )

      Product.createProduct(
        "Rural Product",
        "For the rural dog",
        5,
        AnimalType.Dog,
        AnimalSize.Small
      )

      Product.createProduct(
        "Outdoor Product",
        "For the adventure-minded cat",
        4,
        AnimalType.Cat,
        AnimalSize.Small
      )

      Product.createProduct(
        "Indoor Product",
        "For the homebody cat",
        4,
        AnimalType.Cat,
        AnimalSize.Large
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
}
