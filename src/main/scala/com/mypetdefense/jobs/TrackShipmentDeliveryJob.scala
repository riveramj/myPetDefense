package com.mypetdefense.jobs

import java.text.SimpleDateFormat
import java.time.{LocalDate, ZoneId}
import java.util.Date

import com.mypetdefense.model.ShipmentStatus._
import com.mypetdefense.model._
import dispatch.Defaults._
import dispatch._
import net.liftweb.common._
import net.liftweb.json.Xml.toJson
import net.liftweb.json._
import net.liftweb.mapper._
import net.liftweb.util.Helpers.tryo
import org.quartz._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure => TryFail, _}

class TrackShipmentDeliveryJob extends ManagedJob {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

    val currentDate       = LocalDate.now()
    val alertDeliveryDate = currentDate.plusDays(7)
    val uspsApiUrl        = url("https://secure.shippingapis.com/ShippingAPI.dll").secure

    val retryAttempts = 10

    def convertDateFormat(date: Date) = {

      date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
    }

    def generateTrackingXml(trackingNumber: String) = {
      s"<TrackFieldRequest USERID='840MYPET0182'><Revision>1</Revision><ClientIp>174.49.109.237</ClientIp><SourceId>My Pet Defense</SourceId><TrackID ID='${trackingNumber}'></TrackID></TrackFieldRequest>"
    }

    def trackingNumberResponse(trackingNumber: String) = {
      Http
        .default(
          uspsApiUrl << Map(
            "API" -> "TrackV2",
            "XML" -> generateTrackingXml(trackingNumber)
          ) OK as.xml.Elem
        )
        .either
        .map {
          case Left(throwable) =>
            logger.error(s"taxjar error: ${throwable}")
            Failure("Error occured while talking to usps tracking API.", Full(throwable), Empty)
          case Right(possibleTrackingResponse) =>
            Full(possibleTrackingResponse)
        }
    }

    def rawTrackingNumberResponse(
        trackingNumber: String,
        attemptsLeft: Int
    ): Box[scala.xml.Elem] = {
      Try(Await.result(trackingNumberResponse(trackingNumber), 1 seconds)) match {
        case Success(trackingResponse) =>
          trackingResponse
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawTrackingNumberResponse(trackingNumber, attemptsLeft - 1)
          else {
            logger.error(
              s"Timeout occured while talking to USPS tracking for shipment tracking with ${throwable}"
            )
            Failure(
              "Timeout occured while talking to USPS tracking for shipment tracking.",
              Full(throwable),
              Empty
            )
          }

      }
    }

    def createShipmentEvent(
        shipment: Shipment,
        title: String,
        description: String
    ) = {
      val subscription = shipment.subscription.obj
      val user         = subscription.flatMap(_.user.obj)

      val existingEvent = Event.find(
        By(Event.shipment, shipment),
        NotBy(Event.eventStatus, EventStatus.Resolved)
      )

      if (existingEvent.isEmpty) {
        Event.createEvent(
          user = user,
          subscription = subscription,
          shipment = Full(shipment),
          eventType = EventType.Shipping,
          title = title,
          details = description,
          eventDate = shipment.dateShipped.get
        )
      }
    }

    val recentShipments = Shipment.findAll(
      NotBy(Shipment.trackingNumber, ""),
      NotNullRef(Shipment.trackingNumber),
      NotBy(Shipment.shipmentStatus, Delivered),
      NotBy(Shipment.shipmentStatus, Refused),
      NotBy(Shipment.shipmentStatus, FailedDelivery),
      NotBy(Shipment.shipmentStatus, Other),
      OrderBy(Shipment.dateProcessed, Ascending),
      MaxRows(400)
    )

    recentShipments.map { shipment =>
      val trackingNumber = shipment.trackingNumber.get

      val trackingResponse = rawTrackingNumberResponse(trackingNumber, retryAttempts)

      val statuses = ({
        for {
          possibleResponse <- trackingResponse.toList
          tracking = toJson(possibleResponse)
        } yield {
          val summary = tryo(
            (tracking \ "TrackResponse" \ "TrackInfo" \ "TrackSummary" \ "EventCode")
              .extract[String]
          ).openOr("")

          val statuses = tryo(
            (tracking \ "TrackResponse" \ "TrackInfo" \ "TrackDetail" \ "EventCode")
              .extract[List[String]]
          ).openOr(Nil)

          val singleStatus = {
            if (statuses.isEmpty)
              tryo(
                (tracking \ "TrackResponse" \ "TrackInfo" \ "TrackDetail" \ "EventCode")
                  .extract[String]
              ).openOr("")
            else
              ""
          }

          val notStatus = {
            if (statuses.isEmpty) {
              val fullDescription = tryo(
                (tracking \ "TrackResponse" \ "TrackInfo" \ "Error" \ "Description").extract[String]
              ).openOr("")

              if (fullDescription.contains("not yet available"))
                "GA"
              else
                ""
            } else
              ""
          }

          summary +: singleStatus +: notStatus +: statuses
        }
      }).flatten.filter(!_.isEmpty)

      val (shipmentStatus, deliveryNotes) = statuses match {
        case refused if statuses.contains("21") =>
          (FailedDelivery, "No Such Number")

        case refused if statuses.contains("22") =>
          (FailedDelivery, "Insufficient Address")

        case refused if statuses.contains("23") =>
          (Refused, "Moved, Left No Address")

        case refused if statuses.contains("04") =>
          (Refused, "")

        case undeliverable if statuses.contains("05") =>
          (FailedDelivery, "Undeliverable as Addressed")

        case other if statuses.contains("29") =>
          (FailedDelivery, "Return to Sender")

        case delivered if statuses.contains("01") =>
          (Delivered, "")

        case delivered if statuses.contains("DX") =>
          (DelayedDelivery, "")

        case inTransit
            if statuses
              .intersect(
                List("PC", "OF", "07", "NT", "U1", "T1", "A1", "L1", "03", "10", "OA", "SF")
              )
              .nonEmpty =>
          (InTransit, "")

        case labelCreated if statuses.intersect(List("GX", "MA", "GA")).nonEmpty =>
          (LabelCreated, "")

        case _ =>
          (Other, "")
      }

      val updatedShipment =
        shipment.shipmentStatus(shipmentStatus).deliveryNotes(deliveryNotes).saveMe

      shipmentStatus match {
        case InTransit => {
          val dateShipped = convertDateFormat(shipment.dateShipped.get)

          if (dateShipped.isBefore(currentDate.minusDays(7))) {
            createShipmentEvent(
              updatedShipment,
              "Shipment not delivered yet",
              s"Shipment status is '${shipmentStatus}'. Shipment was mailed on ${dateShipped} but still not delivered as of ${currentDate}."
            )
          }
        }

        case LabelCreated => {
          val dateShipped = convertDateFormat(shipment.dateShipped.get)

          if (dateShipped.isBefore(currentDate.minusDays(20))) {
            val oldShipment = shipment.shipmentStatus(Other).deliveryNotes("Old label.").saveMe

            createShipmentEvent(
              oldShipment,
              "Old label in system",
              s"Ship date of shipment is ${shipment.dateShipped}. Thats older than 20 days. Marked as 'Other'."
            )
          } else if (dateShipped.isBefore(currentDate.minusDays(3))) {
            createShipmentEvent(
              updatedShipment,
              "Shipment not in transit yet",
              s"Label was created but still not in transit as of ${currentDate}."
            )
          }
        }

        case Refused | FailedDelivery =>
          createShipmentEvent(
            updatedShipment,
            "Shipment failed delivery.",
            s"Shipment status is '${shipmentStatus}'. Delivery notes are '${deliveryNotes}'."
          )

        case Other =>
          createShipmentEvent(
            updatedShipment,
            "Shipment Status marked as 'Other'.",
            s"Shipment status is 'Other'. Needs manual investigation. Statuses are: '${statuses}'"
          )

        case _ =>
      }
    }
  }
}

object DailyTrackShipmentDeliveryJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[TrackShipmentDeliveryJob])
    .withIdentity("DailyTrackShipmentDeliveryJob")
    .build()

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("DailyTrackShipmentDeliveryJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */5 0-8 ? * MON-SAT *"))
    .build()
}

object FrequentTrackShipmentDeliveryJob extends TriggeredJob {
  val detail: JobDetail = JobBuilder
    .newJob(classOf[TrackShipmentDeliveryJob])
    .withIdentity("FrequentTrackShipmentDeliveryJob")
    .build

  val trigger: Trigger = TriggerBuilder
    .newTrigger()
    .withIdentity("FrequentTrackShipmentDeliveryJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
