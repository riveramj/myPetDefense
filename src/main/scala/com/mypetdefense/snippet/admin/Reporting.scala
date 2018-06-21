package com.mypetdefense.snippet 
package admin

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._

import com.mypetdefense.model._
import com.mypetdefense.service.ReportingService

object Reporting extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Reporting") / "admin" / "reporting" >>
    adminUser >>
    loggedIn
}

class Reporting extends Loggable {
  val agencies = Agency.findAll().map(_.name.get)

  def convertToPercentage(percent: Double) = f"${percent*100}%.1f%%"

  def render = {
    ".reporting [class+]" #> "current" &
    ".agency" #> agencies.map { agencyName =>

      val users = ReportingService.getUsersForAgency(agencyName)
      val subscriptions = ReportingService.getSubscriptions(users)
      val cancellations = ReportingService.cancelsByShipment(subscriptions)
      val shipments = ReportingService.getShipments(subscriptions)
      val averageShipments = shipments.size.toDouble/subscriptions.size.toDouble
      var totalCancels = 0

      ".agency-name *" #> agencyName &
      ".shipments *" #> f"$averageShipments%.1f" & 
      ".cancel-detail" #> cancellations.toSeq.sorted.map { case (shipmentCount, shipmentCancellations) =>

        val startingCustomers = users.size - totalCancels
        totalCancels = totalCancels + shipmentCancellations
        val remainingCustomers = startingCustomers - shipmentCancellations

        val cancellationRate = (shipmentCancellations/startingCustomers.toDouble)
        val rentionRate = 1.0 - cancellationRate

        ".shipment-count *" #> shipmentCount & 
        ".customer-start *" #> startingCustomers &
        ".cancellations *" #> shipmentCancellations &
        ".cancel-rate *" #> convertToPercentage(cancellationRate) &
        ".retention-rate *" #> convertToPercentage(rentionRate) &
        ".customer-remaining *" #> remainingCustomers 
      }
    }
  }
}
