package com.mypetdefense.snippet
package admin 

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.common._
import net.liftweb.http._
  import js.JsCmds._
import net.liftweb.mapper.{By, NullRef}

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.Paths._
import com.mypetdefense.util._
import com.mypetdefense.actor._
import com.mypetdefense.service._

object EventsDashboard extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Events Dashboard") / "admin" / "events-dashboard" >>
    mpdAdmin >>
    loggedIn
}

class EventsDashboard extends Loggable {
  val dateFormat = new SimpleDateFormat("MM/dd")
  
  var eventActionRenderer: Box[IdMemoizeTransform] = Empty
  var unresolvedEvents = Event.unresolvedEvents

  def actionOnEvent(
    event: Event,
    eventNotes: String,
    eventStatus: EventStatus.Value,
    renderer: IdMemoizeTransform
  )() = {
    val updatedEvent = event.notes(eventNotes).eventStatus(eventStatus)

    if (eventStatus == EventStatus.Resolved)
      updatedEvent.resolutionDate(new Date()).saveMe
    else
      updatedEvent.saveMe
    
    unresolvedEvents = Event.unresolvedEvents

    renderer.setHtml
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".event-dashboard [class+]" #> "current" &
    ".event-details" #> SHtml.idMemoize { eventActionRenderer =>
      ".event" #> unresolvedEvents.sortBy(_.eventDate.get.getTime).map { event =>
        var notes = event.notes.get
        val trackingNumber = event.shipment.obj.map(_.trackingNumber.get)

        ".event-date *" #> dateFormat.format(event.eventDate.get) &
        ".title *" #> event.title.get &
        ".details *" #> event.details.get &
        ".account-email *" #> event.user.obj.map(_.email.get) &
        ".tracking-number a *" #> trackingNumber &
        ".tracking-number a [href]" #> s"https://tools.usps.com/go/TrackConfirmAction.action?tLabels=${trackingNumber.openOr("")}" &
        ".event-type *" #> event.eventType.get.toString &
        ".notes" #> SHtml.ajaxTextarea(notes, notes = _) & 
        ".resolve" #> SHtml.ajaxSubmit("Resolved", () => actionOnEvent(event, notes, EventStatus.Resolved, eventActionRenderer)) &
        ".set-pending" #> SHtml.ajaxSubmit("Pending", () => actionOnEvent(event, notes, EventStatus.Pending, eventActionRenderer))
      }
    }
  }
}
