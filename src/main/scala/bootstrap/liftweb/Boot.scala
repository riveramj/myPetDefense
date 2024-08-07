package bootstrap.liftweb

import com.mypetdefense.actor.{BootJobActor, EmailCustomersShutdownCancel}
import com.mypetdefense.jobs.JobManager
import com.mypetdefense.snippet._
import com.mypetdefense.util._
import com.stripe.Stripe
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util.Props

import java.util.{Locale, TimeZone}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot(): Unit = {
    MailConfig.init

    DbSetup.setup
    DbSetup.migrateTables

    //DataLoader.loadProducts
    //DataLoader.loadAdmin
    //DataLoader.createProducts
    //DataLoader.defaultSaleCoupons
    //DataLoader.connectBoxToPet
    //DataLoader.markUpgradedSubscriptions
    //DataLoader.upgradeInsert
    //DataLoader.clearRecentShipments()

    //DataLoader.loadPetlandInsert
    //DataLoader.createBasicExistingBoxes
    //DataLoader.dataCleanUp()
    //DataLoader.removeDupUsers()
    //DataLoader.createMissingCatBoxes()
    //DataLoader.cancellationDataSync()
    //DataLoader.createMissingDogBoxes()
    //ReportingService.getPetlandCustomersWithStats
    //DataLoader.createEmailReports()
    //DataLoader.markSupplements
    //DataLoader.addMonthlyTPPAgentReport()

    //DataLoader.checkUpgradesAgainstBoxes

    //DataLoader.createEmailReports()
    //DataLoader.markSupplements
    //DataLoader.upgradeSubscriptionBoxDetails()
    //DataLoader.rebuildSubscriptionUpgrades()
    //DataLoader.subscriptionBoxCheck()
    //DataLoader.connectCancelledUsersToSubscription()
    //DataLoader.cancelBoxesForCancelledPets()
    //DataLoader.createResetPasswordEmailTemplate

    //DataLoader.updateChewCounts()
    //DataLoader.createProducts
    //DataLoader.migrateTo30DaySupply()
    //DataLoader.createMissingProbiotic
    //DataLoader.dentalProductCleanup
    //DataLoader.createCancelledSubForUser

    // where to search snippet
    LiftRules.addToPackages("com.mypetdefense")

    LiftRules.setSiteMap(Paths.siteMap)

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) => Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    // set DocType to HTML5
    LiftRules.htmlProperties.default.set((r: Req) => Html5Properties(r.userAgent))

    LiftRules.statelessDispatch.append(StripeHook)
    LiftRules.statelessDispatch.append(TPPApi)

    //BootJobActor ! MigrateStripeProducts

    BootJobActor ! EmailCustomersShutdownCancel
  }

  //Bundles
  LiftRules.snippets.append(Bundles.snippetHandlers)

  // In cases where we have an AJAX request for IE with an uploaded file, we
  // assume we served through an iframe (a fairly safe assumption) and serve
  // up the response with a content type of text/plain so that IE does not
  // attempt to save the file.
  LiftRules.responseTransformers.append { resp =>
    (for (req <- S.request) yield {
      resp.toResponse match {
        case InMemoryResponse(data, headers, cookies, code)
            if req.param("liftIFrameUpload") === req.path.wholePath.last &&
              req.path.wholePath.head == LiftRules.liftPath =>
          val contentlessHeaders = headers.filterNot(_._1.toLowerCase == "content-type")
          InMemoryResponse(
            data,
            ("Content-Type", "text/plain; charset=utf-8") :: contentlessHeaders,
            cookies,
            code
          )
        case _ => resp
      }
    }) openOr resp
  }

  TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))
  Locale.setDefault(Locale.US)

  // Initialize Stripe SDK
  Stripe.apiKey = Props.get("secret.key") openOr ""

  // startup quartz scheduler
  JobManager.init()

  //Lift CSP settings see http://content-security-policy.com/ and
  //Lift API for more information.
  LiftRules.securityRules = () => {
    SecurityRules(content =
      Some(
        ContentSecurityPolicy(
          scriptSources = List(
            ContentSourceRestriction.Self,
            ContentSourceRestriction.UnsafeInline,
            ContentSourceRestriction.Host("https://ajax.googleapis.com"),
            ContentSourceRestriction.Host("https://connect.facebook.net"),
            ContentSourceRestriction.Host("https://facebook.com"),
          ),
          styleSources = List(
            ContentSourceRestriction.Self,
            ContentSourceRestriction.UnsafeInline,
            ContentSourceRestriction.Host("https://fonts.googleapis.com"),
            ContentSourceRestriction.Host("https://cdn.jsdelivr.net"),
          ),
          fontSources = List(
            ContentSourceRestriction.Self,
            ContentSourceRestriction.Host("https://fonts.googleapis.com"),
            ContentSourceRestriction.Host("https://fonts.gstatic.com"),
            ContentSourceRestriction.Host("data:")
          ),
          frameSources = List(
            ContentSourceRestriction.Host("https://www.youtube.com"),
            ContentSourceRestriction.Host("https://www.facebook.com")
          ),
          connectSources = List(
            ContentSourceRestriction.Host("https://facebook.com"),
            ContentSourceRestriction.UnsafeInline,
            ContentSourceRestriction.Self,
            ContentSourceRestriction.All
          ),
        )
      )
    )
  }
}
