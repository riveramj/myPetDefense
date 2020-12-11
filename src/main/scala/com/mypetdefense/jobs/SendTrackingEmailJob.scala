package com.mypetdefense.jobs

import java.util.Date

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.{InventoryService, ParentService, ShipStationService}
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import org.quartz._

class SendTrackingEmailJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    val labels = ShipStationService.getYesterdayShipments().map(_.shipments).openOr(Nil)

    labels map { label =>
      val orderNumber      = tryo(label.orderNumber.toLong).openOr(0L)
      val possibleShipment = Shipment.find(By(Shipment.shipmentId, orderNumber))

      for {
        shipment <- possibleShipment
        if shipment.dateShipped.get == null
        subscription <- shipment.subscription.obj
        user         <- subscription.user.obj
        address <- Address
                    .find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))
      } yield {
        val nameAddress = {
          s"""${user.name}
          |${address.street1.get}
          |${address.street2.get}
          |${address.city.get}, ${address.state.get} ${address.zip.get}""".stripMargin
            .replaceAll("\n\n", "\n")
        }

        shipment
          .dateShipped(new Date())
          .address(nameAddress)
          .trackingNumber(label.trackingNumber)
          .saveMe

        ParentService.updateNextShipDate(subscription)
        InventoryService.deductShipmentItems(shipment)

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
  val detail: JobDetail = JobBuilder
    .newJob(classOf[SendTrackingEmailJob])
    .withIdentity("DailyTrackingEmailJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyTrackingEmailJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 0 8 ? * * *"))
    .build()
}

object FrequentTrackingEmailJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[SendTrackingEmailJob])
    .withIdentity("FrequentTrackingEmailJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentTrackingEmailJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
