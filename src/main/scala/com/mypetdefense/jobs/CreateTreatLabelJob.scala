package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.service.ShipStationService
import dispatch.Defaults._
import net.liftweb.common._
import net.liftweb.mapper._
import org.quartz._

import scala.util.{Failure => TryFail, Success => TrySuccess}

class CreateTreatLabelJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val treatOrders = TreatOrder.findAll(
      By(TreatOrder.shipStationOrderId, 0),
      By(TreatOrder.shipmentStatus, ShipmentStatus.Paid)
    )

    treatOrders map { treatOrder =>
      val shipStationOrder = ShipStationService.createShipStationTreatOrder(treatOrder)

      shipStationOrder.onComplete {
        case TrySuccess(Full(order)) =>
          treatOrder
            .shipStationOrderId(order.orderId)
            .shipmentStatus(ShipmentStatus.LabelCreated)
            .saveMe

        case TrySuccess(shipStationFailure) =>
          logger.error(s"create order failed with shipStation error:")
          logger.error(shipStationFailure)
          logger.error(s"email is ${treatOrder.email.get}")

        case TryFail(throwable: Throwable) =>
          logger.error(s"create order failed with other error: ${throwable}")
          logger.error(s"email is ${treatOrder.email.get}")
          throwable
      }
    }
  }
}

object HalfHourCreateTreatLabelJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[CreateTreatLabelJob])
    .withIdentity("HalfHourCreateTreatLabelJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("HalfHourCreateTreatLabelJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 30 * ? * * *"))
    .build()
}

object FrequentCreateTreatLabelJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[CreateTreatLabelJob])
    .withIdentity("FrequentCreateTreatLabelJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentCreateTreatLabelJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 1 minutes
    .build
}
