package com.mypetdefense.generator

case class SubscriptionUpgradeChainData(
  subscription: SubscriptionCreateGeneratedData,
  user: UserCreateGeneratedData,
  pets: List[PetData],
  subscriptionUpgrade: SubscriptionUpgradeCreateGeneratedData
)
