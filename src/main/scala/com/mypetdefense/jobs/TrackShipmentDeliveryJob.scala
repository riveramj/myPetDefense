package com.mypetdefense.jobs

import net.liftweb._ 
  import common._
  import json._
    import Extraction._
  import Xml.{toJson, toXml}
  import mapper._
  import util.Helpers.tryo

import dispatch._, Defaults._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

import scala.collection.concurrent.TrieMap
import scala.util.{Failure => TryFail, _}

import org.quartz.{CronScheduleBuilder, TriggerBuilder, JobBuilder, JobExecutionContext}

import com.mypetdefense.model._

class TrackShipmentDeliveryJob extends ManagedJob {
  implicit val formats = DefaultFormats

  def execute(context: JobExecutionContext): Unit = executeOp(context) {
    val uspsApiUrl = url("https://secure.shippingapis.com/ShippingAPI.dll").secure

    val retryAttempts = 10
    
    def generateTrackingXml(trackingNumber: String) = {
      s"<TrackFieldRequest USERID='840MYPET0182'><Revision>1</Revision><ClientIp>174.49.109.237</ClientIp><SourceId>My Pet Defense</SourceId><TrackID ID='${trackingNumber}'></TrackID></TrackFieldRequest>"
    }

    def trackingNumberResponse(trackingNumber: String) = {
      Http.default(uspsApiUrl << Map(
        "API" -> "TrackV2",
        "XML" -> generateTrackingXml(trackingNumber)
      ) OK as.xml.Elem).either.map {
        case Left(throwable) =>
          logger.error(s"taxjar error: ${throwable}")
          Failure("Error occured while talking to usps tracking API.", Full(throwable), Empty)
        case Right(possibleTrackingResponse) =>
          Full(possibleTrackingResponse)
      }
    }

    def rawTrackingNumberResponse(trackingNumber: String, attemptsLeft: Int): Box[scala.xml.Elem] = {
      Try(Await.result(trackingNumberResponse(trackingNumber), 1 seconds)) match {
        case Success(trackingResponse) => 
          trackingResponse
        case TryFail(throwable: Throwable) =>
          if (attemptsLeft > 0)
            rawTrackingNumberResponse(trackingNumber, attemptsLeft - 1)
          else {
            logger.error(s"Timeout occured while talking to USPS tracking for shipment tracking with ${throwable}")
            Failure("Timeout occured while talking to USPS tracking for shipment tracking.", Full(throwable), Empty)
          }

      }
    }

    val recentShipments = Shipment.findAll(
      NotBy(Shipment.trackingNumber, ""),
      NotBy(Shipment.shipmentStatus, ShipmentStatus.Delivered),
      NotBy(Shipment.shipmentStatus, ShipmentStatus.Refused),
      NotBy(Shipment.shipmentStatus, ShipmentStatus.FailedDelivery),
      MaxRows(100)
    )

    recentShipments.map { shipment =>
      val trackingNumber = shipment.trackingNumber.get

      val trackingResponse = rawTrackingNumberResponse(trackingNumber, retryAttempts)

      val statuses = ({
        for {
          possibleResponse <- trackingResponse.toList
          tracking = toJson(possibleResponse)
        } yield {
          val summary = tryo((tracking \ "TrackResponse" \ "TrackInfo" \  "TrackSummary" \ "EventCode").extract[String]).openOr("")

          val statuses = tryo((tracking \ "TrackResponse" \ "TrackInfo" \  "TrackDetail" \ "EventCode").extract[List[String]]).openOr(Nil)

          val singleStatus = {
            if (statuses.isEmpty)
              tryo((tracking \ "TrackResponse" \ "TrackInfo" \  "TrackDetail" \ "EventCode").extract[String]).openOr("")
            else
              ""
          }

          summary +: singleStatus +: statuses
        }
      }).flatten

      val (shipmentStatus, deliveryNotes) = statuses match {
        case refused if statuses.contains("21") =>
          (ShipmentStatus.FailedDelivery, "No Such Number")

        case refused if statuses.contains("22") =>
          (ShipmentStatus.FailedDelivery, "Insufficient Address")

        case refused if statuses.contains("23") =>
          (ShipmentStatus.Refused, "Moved, Left No Address")

        case refused if statuses.contains("04") =>
          (ShipmentStatus.Refused, "")

        case undeliverable if statuses.contains("05") =>
          (ShipmentStatus.FailedDelivery, "Undeliverable as Addressed")

        case other if statuses.contains("29") =>
          (ShipmentStatus.FailedDelivery, "Return to Sender")

        case delivered if statuses.contains("01") =>
          (ShipmentStatus.Delivered, "")

        case delivered if statuses.contains("DX") =>
          (ShipmentStatus.DelayedDelivery, "")

        case inTransit if statuses.intersect(List("L1", "03", "10", "OA", "SF")).nonEmpty =>
          (ShipmentStatus.InTransit, "")

        case labelCreated if statuses.intersect(List("GX", "MA", "GA")).nonEmpty =>
          (ShipmentStatus.LabelCreated, "")

        case _ =>
          (ShipmentStatus.Other, "")
      }

      shipment.shipmentStatus(shipmentStatus).deliveryNotes(deliveryNotes).saveMe
    }
  }   
}

object DailyTrackShipmentDeliveryJob extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[TrackShipmentDeliveryJob])
    .withIdentity("DailyTrackShipmentDeliveryJob")
    .build()

    val trigger = TriggerBuilder.newTrigger()
    .withIdentity("DailyTrackShipmentDeliveryJobTrigger")
    .startNow()
    .withSchedule(CronScheduleBuilder.cronSchedule("0 5 * ? * MON-SAT *"))
    .build()
}

object FrequentTrackShipmentDeliveryJobb extends TriggeredJob {
  val detail = JobBuilder.newJob(classOf[TrackShipmentDeliveryJob])
    .withIdentity("FrequentTrackShipmentDeliveryJob")
    .build

  val trigger = TriggerBuilder.newTrigger()
    .withIdentity("FrequentTrackShipmentDeliveryJobTrigger")
    .startNow
    .withSchedule(CronScheduleBuilder.cronSchedule("0 */1 * ? * *")) // fire every 5 minutes
    .build
}
