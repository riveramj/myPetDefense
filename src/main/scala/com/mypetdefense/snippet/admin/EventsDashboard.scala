package com.mypetdefense.snippet
package admin

import java.text.SimpleDateFormat
import java.time.ZonedDateTime

import com.mypetdefense.AppConstants.DefaultTimezone
import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

object EventsDashboard extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Events Dashboard") / "admin" / "events-dashboard" >>
    mpdAdmin >>
    loggedIn
}

class EventsDashboard extends Loggable {
  val dateFormat = new SimpleDateFormat("MM/dd/yyyy")

  var eventActionRenderer: Box[IdMemoizeTransform] = Empty
  var unresolvedEvents: List[Event]                = Event.unresolvedEvents

  def actionOnEvent(
      event: Event,
      eventNotes: String,
      eventStatus: EventStatus.Value,
      renderer: IdMemoizeTransform
  )(): JsCmd = {
    val updatedEvent = event.notes(eventNotes).eventStatus(eventStatus)

    if (eventStatus == EventStatus.Resolved)
      updatedEvent.resolutionDate(ZonedDateTime.now(DefaultTimezone)).saveMe
    else
      updatedEvent.saveMe

    unresolvedEvents = Event.unresolvedEvents

    renderer.setHtml
  }

  def getEmail(event: Event): Box[String] = {
    val parent = event.user.obj

    if (parent.map(_.status == Status.Cancelled).openOr(false)) {
      event.user.obj.flatMap(user =>
        CancelledUser.find(By(CancelledUser.user, user.userId.get)).map(_.email.get)
      )
    } else {
      event.user.obj.map(_.email.get)
    }
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".event-dashboard [class+]" #> "current" &
        ".event-details" #> SHtml.idMemoize { eventActionRenderer =>
          ".event" #> unresolvedEvents.sortBy(_.eventDate.get.toInstant.toEpochMilli).map { event =>
            var notes          = event.notes.get
            val trackingNumber = event.shipment.obj.map(_.trackingNumber.get)

            ".event-date *" #> dateFormat.format(event.eventDate.get) &
              ".title *" #> event.title.get &
              ".details *" #> event.details.get &
              ".account-email *" #> getEmail(event) &
              ".tracking-number a *" #> trackingNumber &
              ".tracking-number a [href]" #> s"https://tools.usps.com/go/TrackConfirmAction.action?tLabels=${trackingNumber
                .openOr("")}" &
              ".event-type *" #> event.eventType.get.toString &
              ".notes" #> SHtml.ajaxTextarea(notes, notes = _) &
              ".resolve" #> SHtml.ajaxSubmit(
                "Resolved",
                () => actionOnEvent(event, notes, EventStatus.Resolved, eventActionRenderer)
              ) &
              ".set-pending" #> SHtml.ajaxSubmit(
                "Pending",
                () => actionOnEvent(event, notes, EventStatus.Pending, eventActionRenderer)
              )
          }
        }
  }
}
