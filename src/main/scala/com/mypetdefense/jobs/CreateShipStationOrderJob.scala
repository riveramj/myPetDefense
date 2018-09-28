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

class CreateShipStationOrderJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    println("in job =====")
    
    def sameDateComparison(date1: Date, date2: Date) = {
      val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

      dateFormat.format(date1) == dateFormat.format(date2)
    }

    val newShipments = Shipment.findAll(NullRef(Shipment.shipstationOrderKey))

    newShipments.map(println(_))

    for {
      shipment <- newShipments
      subscription <- shipment.subscription.obj
      user <- subscription.user.obj
    } yield { 
      val shipStationOrder = ShipStationService.createShipStationOrder(shipment, user)

      shipStationOrder.onComplete {
        case TrySuccess(Full(order)) =>
          val refreshedShipment = shipment.refresh
          val foo = refreshedShipment.map(_.shipstationOrderKey(order.orderKey.getOrElse("")).saveMe)
          println(foo)

          if (!sameDateComparison(
            new Date(),
            shipment.expectedShipDate.get
          )) {
            ShipStationService.holdOrderUntil(
              order.orderId,
              shipment.expectedShipDate.get
            ).onComplete {
              case TrySuccess(Full(_)) =>

              case TrySuccess(shipStationFailure) =>
                logger.error(s"hold order failed with shipStation error: ${shipStationFailure}")

              case TryFail(throwable: Throwable) =>
                logger.error(s"hold order failed with other error: ${throwable}")
            }
          }

              case TrySuccess(shipStationFailure) =>
                logger.error(s"create order failed with shipStation error: ${shipStationFailure}")
                shipStationFailure

              case TryFail(throwable: Throwable) =>
                logger.error(s"create order failed with other error: ${throwable}")
                throwable
      }
    }
  }
}

object HalfHourCreateOrderJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[CreateShipStationOrderJob])
    .withIdentity("HalfHourCreateOrderJob")
    .build()

    val trigger = TriggerBuilder.newTrigger()
      .withIdentity("HalfHourCreateOrderJobTrigger")
      .startNow()
      .withSchedule(CronScheduleBuilder.cronSchedule("0 30 * ? * * *"))
      .build()
}

object FrequentCreateOrderJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[CreateShipStationOrderJob])
    .withIdentity("FrequentCreateOrderJob")
    .build

    val trigger = TriggerBuilder.newTrigger()
      .withIdentity("FrequentCreateOrderJobTrigger")
      .startNow
      .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 1 minutes
      .build
}
