package com.mypetdefense.generator

case class PetsAndShipmentChainData(
    user: UserCreateGeneratedData,
    subscriptionCreateGeneratedData: SubscriptionCreateGeneratedData,
    shipmentCreateGeneratedData: List[ShipmentCreateGeneratedData],
    pets: List[PetData]
)
