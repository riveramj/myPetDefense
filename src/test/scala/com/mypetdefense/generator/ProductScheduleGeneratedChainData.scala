package com.mypetdefense.generator

import java.util.Date

case class ProductScheduleGeneratedChainData(
    productData: List[ProductGeneratedData],
    scheduleStartData: Date
)
