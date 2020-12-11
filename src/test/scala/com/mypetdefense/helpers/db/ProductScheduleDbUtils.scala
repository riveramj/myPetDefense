package com.mypetdefense.helpers.db

import java.util.Date

import com.mypetdefense.model.{Product, ProductSchedule}

object ProductScheduleDbUtils {
  def createProductSchedule(startDate: Date, products: List[Product]): ProductSchedule = {
    ProductSchedule.createNew(startDate, products)
  }
}
