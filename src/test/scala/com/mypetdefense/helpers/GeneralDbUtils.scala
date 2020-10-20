package com.mypetdefense.helpers

import com.mypetdefense.model._

object GeneralDbUtils {

  def clearTables(): Unit = {
    User.findAll().map(_.delete_!)
    Subscription.findAll().map(_.delete_!)
  }

}
