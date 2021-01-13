package com.mypetdefense.generator

import java.time.ZonedDateTime

case class ProductScheduleGeneratedChainData(
    productData: List[ProductGeneratedData],
    scheduleStartData: ZonedDateTime
)
