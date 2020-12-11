package com.mypetdefense.helpers

import bootstrap.liftweb.{DbSetup, MailConfig}
import com.mypetdefense.model._
import net.liftweb.mapper.Schemifier

object BootUtil {

  def bootForTests(): Unit = {
    MailConfig.init

    DbSetup.setup

    DbSetup.migrateTables
  }

}
