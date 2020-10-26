package com.mypetdefense.generator

import com.mypetdefense.model.{AnimalSize, AnimalType}

case class PetChainData(
    user: UserCreateGeneratedData,
    petData: List[PetData]
)

case class PetData(
    petName: String,
    petType: AnimalType.Value,
    petSize: AnimalSize.Value
)
