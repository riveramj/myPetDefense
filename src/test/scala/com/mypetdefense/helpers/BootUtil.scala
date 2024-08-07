package com.mypetdefense.helpers

import java.util.{Locale, TimeZone}

import bootstrap.liftweb.{DbSetup, MailConfig}
import com.mypetdefense.model._
import net.liftweb.common.Full
import net.liftweb.mapper.Schemifier
import net.liftweb.util.Props

object BootUtil {

  val testDatabase: TestDatabase =
    sys.env.getOrElse("TESTDB", "h2memory").toLowerCase match {
      case "postgresql" | "postgres" => PostgreSQL
      case "h2memory" | "h2mem"      => H2Memory
      case value =>
        throw new IllegalArgumentException(s"TESTDB contains unrecognized value: '$value'")
    }

  lazy val bootOnceForTests: Unit = {
    TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))
    Locale.setDefault(Locale.US)

    setPropsFile()

    MailConfig.init
    DbSetup.setup
    Schemifier.schemify(performWrite = true, Schemifier.infoF _, dbTables: _*)
  }

  private def setPropsFile(): Unit = {
    val propsPath = testDatabase match {
      case PostgreSQL => "props/test.postgresql.props"
      case H2Memory   => "props/test.h2memory.props"
    }

    val propsStream = Thread.currentThread().getContextClassLoader.getResourceAsStream(propsPath)

    Props.whereToLook = () => (propsPath, () => Full(propsStream)) :: Nil
  }

  lazy val dbTables = List(
    User,
    CancelledUser,
    Address,
    Pet,
    FleaTick,
    Shipment,
    Event,
    ShipmentLineItem,
    Subscription,
    SubscriptionBox,
    SubscriptionItem,
    SubscriptionUpgrade,
    Agency,
    Coupon,
    Price,
    GrowthRate,
    Review,
    Survey,
    TreatOrder,
    Insert,
    Product,
    TreatOrderLineItem,
    Packaging,
    TaggedItem,
    Tag,
    AddOnProduct,
    ApiRequestBackup,
    ProductSchedule,
    ProductScheduleItem,
    ActionLog,
    ActionLogDetails,
    WoofTraxOrder,
    EmailReport,
    EmailReportRecord,
    StatisticsSnapshot,
    MandrillTemplate
  )

}
