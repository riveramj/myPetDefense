package com.mypetdefense.jobs

import net.liftweb._
  import common._
  import mapper._
  import util.Helpers._ 

import com.mypetdefense.service.{ShipStationService, ParentService}
import com.mypetdefense.actor._
import com.mypetdefense.model._

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import java.util.Date
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime, Period}
import java.time.format.DateTimeFormatter
import com.mypetdefense.shipstation.{Address => ShipStationAddress, Shipment => ShipStationShipment, _}

class SendTrackingEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    
    val labels = ShipStationService.getYesterdayShipments().map(_.shipments).openOr(Nil)

    labels map { label =>
      val orderNumber = tryo(label.orderNumber.toLong).openOr(0L)
      val possibleShipment = Shipment.find(By(Shipment.shipmentId, orderNumber))

      for {
        shipment <- possibleShipment
          if shipment.dateShipped.get == null
        subscription <- shipment.subscription.obj
        user <- subscription.user.obj
        address <- Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))
      } yield {
        val nameAddress = {
          s"""${user.name}
          |${address.street1.get}
          |${address.street2.get}
          |${address.city.get}, ${address.state.get} ${address.zip.get}""".stripMargin.replaceAll("\n\n", "\n")
        }

        shipment.dateShipped(new Date()).address(nameAddress).trackingNumber(label.trackingNumber).saveMe

        ParentService.updateNextShipDate(subscription, Full(user))

        EmailActor ! SendInvoicePaymentSucceededEmail(
          Full(user),
          Full(subscription),
          shipment.taxPaid.get,
          shipment.amountPaid.get,
          label.trackingNumber
        )
      }
    }
  }
}

object DailyTrackingEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SendTrackingEmailJob])
    .withIdentity("DailyTrackingEmailJob")
    .build()

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailyTrackingEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * * *"))
    .build()
}

object FrequentTrackingEmailJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[SendTrackingEmailJob])
    .withIdentity("FrequentTrackingEmailJob")
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentTrackingEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
