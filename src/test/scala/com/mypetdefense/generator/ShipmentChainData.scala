package com.mypetdefense.generator

case class ShipmentChainData(
    user: UserCreateGeneratedData,
    subscriptionCreateGeneratedData: SubscriptionCreateGeneratedData,
    shipmentCreateGeneratedData: List[ShipmentCreateGeneratedData]
)
