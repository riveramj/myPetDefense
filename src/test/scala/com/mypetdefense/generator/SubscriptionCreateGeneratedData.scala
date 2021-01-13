package com.mypetdefense.generator

import java.time.ZonedDateTime

case class SubscriptionCreateGeneratedData(
    stripeSubscriptionId: String,
    startDate: ZonedDateTime,
    nextShipDate: ZonedDateTime,
    priceCode: String,
    contractLength: Int
)
