package com.mypetdefense.jobs

import com.mypetdefense.model._
import com.mypetdefense.service._
import com.mypetdefense.util.RandomIdGenerator.generateLongId
import dispatch.Defaults._
import net.liftweb.common._
import net.liftweb.mapper._
import org.quartz._

import scala.util.{Failure => TryFail, Success => TrySuccess}

trait CreateShipStationOrderJobTrait extends ManagedJob {

  val shipStationService: ShipStationServiceTrait

  def createShipStationOrders(): Unit = {
    val jobRunId = generateLongId
    val newShipments = Shipment.findAll(
      By(Shipment.shipStationOrderId, 0),
      By(Shipment.shipmentStatus, ShipmentStatus.Paid)
    ).filter(_.subscription.obj.map(_.status.get) == Full(Status.Active))

    logger.info(newShipments.size + s" shipment size begin batch [job-run-id:$jobRunId]")
    var count = 0

    for {
      shipment     <- newShipments.take(10)
      subscription <- shipment.subscription.obj
      user         <- subscription.user.obj
    } yield {
      val thisRun = count
      count = count + 1

      logger.info(
        s"""start processing
           [shipment-id:${shipment.id.get}]
           [subscription-id:${subscription.id.get}]
           [user-id:${user.id.get}]
           [user-email:${user.email.get}]
           [run-in-batch:$thisRun]
           [job-run-id:$jobRunId]"""
      )

      val shipStationOrder =
        shipStationService.createShipStationOrder(shipment, user, subscription, thisRun)

      shipStationOrder.onComplete {
        case TrySuccess(Full(order)) =>
          shipment.reload
            .shipStationOrderId(order.orderId)
            .shipmentStatus(ShipmentStatus.LabelCreated)
            .saveMe
          logger.info(s"[run-in-batch:$thisRun][job-run-id:$jobRunId] successfully done")

        case TrySuccess(Failure(message, error, _)) =>
          Event.createEvent(
            Full(user),
            Full(subscription),
            Full(shipment),
            eventType = EventType.Shipping,
            title = s"create shipstation order failed with shipStation error at run $thisRun",
            details = message
          )
          logger.error(s"[run-in-batch:$thisRun][job-run-id:$jobRunId] done with error $message")
          logger.error(s"""[run-in-batch:$thisRun][job-run-id:$jobRunId]: $error""")

        case TrySuccess(Empty) =>
          Event.createEvent(
            Full(user),
            Full(subscription),
            Full(shipment),
            eventType = EventType.Shipping,
            title = s"shipstation order creation failed with empty result $thisRun",
            details = "shipstation service returned nothing"
          )
          logger.error(s"[run-in-batch:$thisRun][job-run-id:$jobRunId] done with empty result")

        case TryFail(throwable: Throwable) =>
          Event.createEvent(
            Full(user),
            Full(subscription),
            Full(shipment),
            eventType = EventType.Shipping,
            title = "create shipstation order failed with other error",
            details = throwable.getMessage
          )
          logger.error(
            s"[run-in-batch:$thisRun][job-run-id:$jobRunId] done with other error ${throwable.getMessage}"
          )
      }
    }
  }
}

class CreateShipStationOrderJob extends CreateShipStationOrderJobTrait {
  override val shipStationService: ShipStationServiceTrait = ShipStationService
  def execute(context: JobExecutionContext): Unit          = executeOp(context)(createShipStationOrders())
}

object HalfHourCreateOrderJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[CreateShipStationOrderJob])
    .withIdentity("HalfHourCreateOrderJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("HalfHourCreateOrderJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */3 * ? * * *"))
    .build()
}

object FrequentCreateOrderJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[CreateShipStationOrderJob])
    .withIdentity("FrequentCreateOrderJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentCreateOrderJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 1 minutes
    .build
}
