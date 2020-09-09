package com.mypetdefense.jobs

import java.text.SimpleDateFormat
import java.util.Date

import com.mypetdefense.model._
import com.mypetdefense.service.ShipStationService
import dispatch.Defaults._
import net.liftweb.common._
import net.liftweb.mapper._
import org.quartz._

import scala.util.{Failure => TryFail, Success => TrySuccess}

class CreateShipStationOrderJob extends ManagedJob {
  def execute(context: JobExecutionContext): Unit = executeOp(context) {

    def sameDateComparison(date1: Date, date2: Date) = {
      val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

      dateFormat.format(date1) == dateFormat.format(date2)
    }

    val newShipments = Shipment.findAll(
      By(Shipment.shipStationOrderId, 0),
      By(Shipment.shipmentStatus, ShipmentStatus.Paid),
    )

    for {
      shipment <- newShipments
        if (newShipments.size < 400)
      subscription <- shipment.subscription.obj
      user <- subscription.user.obj
    } yield { 
      val shipStationOrder = ShipStationService.createShipStationOrder(shipment, user, subscription)

      shipStationOrder.onComplete {
        case TrySuccess(Full(order)) =>
          shipment.shipStationOrderId(order.orderId).shipmentStatus(ShipmentStatus.LabelCreated).saveMe

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
                logger.error(s"user email is ${user.email.get}")
            }
          }

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

object HalfHourCreateOrderJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder.newJob(classOf[CreateShipStationOrderJob])
    .withIdentity("HalfHourCreateOrderJob")
    .build()

    val trigger: Trigger = TriggerBuilder.newTrigger()
      .withIdentity("HalfHourCreateOrderJobTrigger")
      .startNow()
      .withSchedule(CronScheduleBuilder.cronSchedule("0 */5 * ? * * *"))
      .build()
}

object FrequentCreateOrderJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder.newJob(classOf[CreateShipStationOrderJob])
    .withIdentity("FrequentCreateOrderJob")
    .build

    val trigger: Trigger = TriggerBuilder.newTrigger()
      .withIdentity("FrequentCreateOrderJobTrigger")
      .startNow
      .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 1 minutes
      .build
}
