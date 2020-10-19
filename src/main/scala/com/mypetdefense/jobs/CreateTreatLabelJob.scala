package com.mypetdefense.jobs

import net.liftweb._
import mapper.NullRef
import common._
import mapper._
import util.Helpers._
import com.mypetdefense.service.{ParentService, ShipStationService}
import com.mypetdefense.actor._
import com.mypetdefense.model._
import org.quartz.{
  CronScheduleBuilder,
  JobBuilder,
  JobDetail,
  JobExecutionContext,
  Trigger,
  TriggerBuilder
}
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, LocalDateTime, Period, ZoneId}
import java.time.format.DateTimeFormatter

import com.mypetdefense.shipstation.{
  Address => ShipStationAddress,
  Shipment => ShipStationShipment,
  _
}

import scala.util.{Failure => TryFail, Success => TrySuccess, _}
import dispatch.{Req => DispatchReq, _}
import Defaults._

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
