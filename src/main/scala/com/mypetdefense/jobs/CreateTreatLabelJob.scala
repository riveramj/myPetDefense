package com.mypetdefense.jobs 

import net.liftweb._
  import mapper.NullRef
  import common._
  import mapper._
  import util.Helpers._ 

import com.mypetdefense.service.{ShipStationService, ParentService}
import com.mypetdefense.actor._
import com.mypetdefense.model._

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime, Period}
import java.time.format.DateTimeFormatter
import com.mypetdefense.shipstation.{Address => ShipStationAddress, Shipment => ShipStationShipment, _}
import scala.util.{Failure => TryFail, Success => TrySuccess, _}

import dispatch.{Req => DispatchReq, _} , Defaults._

class CreateTreatLabelJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    
    val treatOrders = TreatOrder.findAll(
      By(TreatOrder.shipStationOrderId, 0),
      By(TreatOrder.shipmentStatus, ShipmentStatus.Paid),
    )

    for {
      treatOrder <- treatOrders
      user <- treatOrder.user.obj
    } yield { 
      val shipStationOrder = ShipStationService.createShipStationTreatOrder(treatOrder, user)

      shipStationOrder.onComplete {
        case TrySuccess(Full(order)) =>
          treatOrder.shipStationOrderId(order.orderId).saveMe

        case TrySuccess(shipStationFailure) =>
          logger.error(s"create order failed with shipStation error:")
          logger.error(shipStationFailure)
          logger.error(s"user email is ${user.email.get}")

        case TryFail(throwable: Throwable) =>
          logger.error(s"create order failed with other error: ${throwable}")
          logger.error(s"user email is ${user.email.get}")
          throwable
      }
    }
  }
}

object HalfHourCreateTreatLabelJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[CreateTreatLabelJob])
    .withIdentity("HalfHourCreateTreatLabelJob")
    .build()

    val trigger = TriggerBuilder.newTrigger()
      .withIdentity("HalfHourCreateTreatLabelJobTrigger")
      .startNow()
      .withSchedule(CronScheduleBuilder.cronSchedule("0 30 * ? * * *"))
      .build()
}

object FrequentCreateTreatLabelJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[CreateTreatLabelJob])
    .withIdentity("FrequentCreateTreatLabelJob")
    .build

    val trigger = TriggerBuilder.newTrigger()
      .withIdentity("FrequentCreateTreatLabelJobTrigger")
      .startNow
      .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 1 minutes
      .build
}

