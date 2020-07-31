package bootstrap.liftweb

import net.liftweb._
  import util._
  import Helpers._
  import mapper._
  import common._
  import http._

import com.mypetdefense.util._
import com.mypetdefense.model._
import com.mypetdefense.snippet._
import com.mypetdefense.service.ReportingService
import com.mypetdefense.jobs.JobManager

import java.util.TimeZone

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    MailConfig.init

    if (!DB.jndiJdbcConnAvailable_?) {

      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }

    Schemifier.schemify(
      true,
      Schemifier.infoF _,
      User,
      CancelledUser,
      Address,
      Pet,
      FleaTick,
      Shipment,
      Event,
      ShipmentLineItem,
      Subscription,
      SubscriptionBox,
      SubscriptionItem,
      Agency,
      Coupon,
      Price,
      GrowthRate,
      Review,
      Survey,
      FriendsFamilyProduct,
      FriendsFamilyOrder,
      FriendsFamilyOrderLineItem,
      TreatOrder,
      ItemReconciliation,
      ReconciliationEvent,
      InventoryItem,
      InventoryChangeAudit,
      Insert,
      InventoryItemPart,
      Product,
      TreatOrderLineItem,
      Packaging,
      TaggedItem,
      Tag,
      AddOnProduct,
      AmazonOrder
    )

    //DataLoader.loadProducts
    DataLoader.loadAdmin
    //DataLoader.createProductAccessKey
    //DataLoader.createProducts
    //DataLoader.createPackaging
    //DataLoader.addWeightToFleaTick
    //DataLoader.addWeightToInsert
    //DataLoader.updateParentNoPets
    //DataLoader.createFriendsFamilyProducts
    //DataLoader.updateShipmentShipStationId
    //DataLoader.updateGeorgiaTaxRates
    //DataLoader.updateProductSku
    //DataLoader.loadWelcomeInserts
    //DataLoader.resetUpcomingBillingCylces
    //DataLoader.createOneSizeCat
    //DataLoader.loadPetlandInsert
    //DataLoader.removeFrontlineProduct
    DataLoader.createProducts
    //DataLoader.replaceChickenTreat
    //DataLoader.createNewPetlandStores
    //DataLoader.createPuppySpot
    //DataLoader.createBoxedTag
    //DataLoader.calculateTax
    //DataLoader.createBoxedTag
    //DataLoader.renameVegTreat
    
    //ReportingService.getPetlandCustomersWithStats

    // where to search snippet
    LiftRules.addToPackages("com.mypetdefense")

    LiftRules.setSiteMap(Paths.siteMap)

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    // set DocType to HTML5
    LiftRules.htmlProperties.default.set((r: Req) =>new Html5Properties(r.userAgent))

    LiftRules.statelessDispatch.append(StripeHook)
    LiftRules.statelessDispatch.append(TPPApi)
    LiftRules.statelessDispatch.append(FriendsFamilyAPI)
    LiftRules.statelessDispatch.append(PromikaAPI)
  }

  //Bundles
  LiftRules.snippets.append(Bundles.snippetHandlers)

  // In cases where we have an AJAX request for IE with an uploaded file, we
  // assume we served through an iframe (a fairly safe assumption) and serve
  // up the response with a content type of text/plain so that IE does not
  // attempt to save the file.
  LiftRules.responseTransformers.append {
    resp =>
      (for (req <- S.request) yield {
        resp.toResponse match {
          case InMemoryResponse(data, headers, cookies, code)
          if req.param("liftIFrameUpload") === req.path.wholePath.last &&
          req.path.wholePath.head == LiftRules.liftPath =>
            val contentlessHeaders = headers.filterNot(_._1.toLowerCase == "content-type")
            InMemoryResponse(data, ("Content-Type", "text/plain; charset=utf-8") :: contentlessHeaders, cookies, code)
          case _ => resp
        }
      }) openOr resp
  }

  LiftRules.supplementalHeaders.default.set(
    List(
      ("X-Lift-Version", LiftRules.liftVersion),
      ("Access-Control-Allow-Origin", "https://promikallcresources.com"),
      ("Access-Control-Allow-Content-Type", "application/json"),
      ("Access-Control-Allow-Methods", "GET")
    )
  )

  TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))

  // startup quartz scheduler
  JobManager.init()

  //Lift CSP settings see http://content-security-policy.com/ and
  //Lift API for more information.
  LiftRules.securityRules = () => {
    SecurityRules(content = Some(ContentSecurityPolicy(
      scriptSources = List(
        ContentSourceRestriction.Self,
        ContentSourceRestriction.Host("https://ajax.googleapis.com")
      ),
      styleSources = List(
        ContentSourceRestriction.Self,
        ContentSourceRestriction.Host("https://fonts.googleapis.com")
      ),
      fontSources = List(
        ContentSourceRestriction.Self,
        ContentSourceRestriction.Host("https://fonts.googleapis.com"),
        ContentSourceRestriction.Host("https://fonts.gstatic.com"),
        ContentSourceRestriction.Host("data:")
      ),
      frameSources = List(
        ContentSourceRestriction.Host("https://www.youtube.com"),
      )
    )))
  }
}
