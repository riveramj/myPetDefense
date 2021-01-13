package com.mypetdefense.helpers.db

import java.time.ZonedDateTime

import com.mypetdefense.model.{Product, ProductSchedule}

object ProductScheduleDbUtils {
  def createProductSchedule(startDate: ZonedDateTime, products: List[Product]): ProductSchedule = {
    ProductSchedule.createNew(startDate, products)
  }
}
