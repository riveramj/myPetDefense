package com.mypetdefense.helpers.db

import java.util.Date

import com.mypetdefense.helpers.Random.generateString
import com.mypetdefense.model.{AnimalSize, AnimalType, Pet, User}
import net.liftweb.common.{Box, Empty}

object PetDbUtils {

  def createPet(
      user: User,
      name: String = generateString,
      animalType: AnimalType.Value,
      size: AnimalSize.Value,
      whelpDate: Box[Date] = Empty,
      breed: String = generateString
  ): Pet = Pet.createNewPet(user, name, animalType, size, whelpDate, breed)

}
