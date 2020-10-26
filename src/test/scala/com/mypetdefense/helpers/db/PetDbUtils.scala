package com.mypetdefense.helpers.db

import com.mypetdefense.generator.PetData
import com.mypetdefense.model.{Pet, User}

object PetDbUtils {
  def createPet(user: User, pet: PetData): Pet =
    Pet.createNewPet(user, pet.petName, pet.petType, pet.petSize)
}
