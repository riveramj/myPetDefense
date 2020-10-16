package com.mypetdefense.generator

import java.util.Date

case class SubscriptionCreateGeneratedData(
    stripeSubscriptionId: String,
    startDate: Date,
    nextShipDate: Date,
    priceCode: String,
    isUpgraded: Boolean,
    contractLength: Int
)
