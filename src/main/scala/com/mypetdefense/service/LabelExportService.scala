package com.mypetdefense.service

import net.liftweb._
  import common._
  import mapper._
  import util._
  import util.Helpers._
  import net.liftweb.http._
    import js.JsCmds._

import com.mypetdefense.model._

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import java.time.{LocalDate, ZoneId, LocalDateTime}
import java.time.format.DateTimeFormatter

object LabelExportService extends Loggable {
  val csvHeaders = "Order ID (required)" ::
                     "Order Date" ::
                     "Order Value" ::
                     "Requested Service" ::
                     "Ship To - Name" ::
                     "Ship To - Company" ::
                     "Ship To - Address 1" ::
                     "Ship To - Address 2" ::
                     "Ship To - Address 3" ::
                     "Ship To - City" ::
                     "Ship To - State/Province" ::
                     "Ship To - Postal Code" ::
                     "Ship To - Country" ::
                     "Ship To - Phone" ::
                     "Ship To - Email" ::
                     "Total Weight in Oz" ::
                     "Dimensions - Length" ::
                     "Dimensions - Width" ::
                     "Dimensions - Height" ::
                     "Notes - From Customer" ::
                     "Notes - Internal" ::
                     "Gift Wrap?" ::
                     "Gift Message" ::
                     Nil
  def legacyInsertCheck(shipment: Shipment) = {
    val subscription = shipment.subscription.obj
    val allShipments = subscription.map(_.shipments.toList).openOr(Nil)

    allShipments.size match {
      case 1 => true
      case _ => false
    }
  }

  def exportNewUspsLabels() = {
    val allShipments = ShipmentService.getCurrentPastDueShipments

    val newLabels = allShipments.filter { shipment =>
      val welcomeInsert_? = tryo(
        shipment.insert.get.contains("Welcome")
      ).openOr(false)
      val nullInsert = tryo(shipment.insert.get == null).openOr(false)

      if (nullInsert) {
        legacyInsertCheck(shipment)
      } else {
        welcomeInsert_?
      }
    }

    exportMPDLabelSet(newLabels)
  }

  def exportExistingUspsLabels() = {
    val allShipments = ShipmentService.getCurrentPastDueShipments

    val existingLabels = allShipments.filter { shipment =>
      val welcomeInsert_? = tryo(
        shipment.insert.get.contains("Welcome")
      ).openOr(false)
      val nullInsert = tryo(shipment.insert.get == null).openOr(false)

      if (nullInsert) {
        !legacyInsertCheck(shipment)
      } else {
        !welcomeInsert_?
      }
    }

    exportMPDLabelSet(existingLabels)
  }

  def exportMPDLabelSet(shipments: List[Shipment]): Box[LiftResponse] = {
    val csvRows: List[List[String]] = {
      val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

      {
        for {
          shipment <- shipments
          subscription <- shipment.subscription.obj
          user <- subscription.user.obj
          address <- Address.find(By(Address.user, user), By(Address.addressType, AddressType.Shipping))
        } yield {
          shipment.shipmentId.get.toString ::
          dateFormat.format(new Date()) ::
          "" ::
          "standard shipping" ::
          user.name ::
          "" ::
          address.street1.get ::
          address.street2.get ::
          "" ::
          address.city.get ::
          address.state.get ::
          address.zip.get ::
          "US" ::
          "" ::
          "" ::
          "4" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          Nil
        }
      }
    }

    createCsvForLabels(csvRows)
  }

  def exportFFLabelSet(orders: List[FriendsFamilyOrder]): Box[LiftResponse] = {
    val csvRows: List[List[String]] = {
      val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

      {
        for {
          order <- orders
        } yield {
          order.orderId.get.toString ::
          dateFormat.format(new Date()) ::
          "" ::
          "standard shipping" ::
          order.name.get ::
          "" ::
          order.street1.get ::
          order.street2.get ::
          "" ::
          order.city.get ::
          order.state.get ::
          order.zip.get ::
          "US" ::
          "" ::
          "" ::
          "4" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          "" ::
          Nil
        }
      }
    }

    createCsvForLabels(csvRows)
  }

  def createCsvForLabels(csvRows: List[List[String]]) = {
    val resultingCsv = (List(csvHeaders) ++ csvRows).map(_.mkString(",")).mkString("\n")

    Some(new InMemoryResponse(
      resultingCsv.getBytes("UTF-8"),
      List(
        "Content-Type" -> "binary/octet-stream",
        "Content-Disposition" -> "attachment; filename=\"shipments.csv\""
        ),
      Nil,
      200
    ))
  }

  def exportFriendsFamilyUspsLabels() = {
    val newOrders = FriendsFamilyOrder.newOrders

    exportFFLabelSet(newOrders)
  }
}
