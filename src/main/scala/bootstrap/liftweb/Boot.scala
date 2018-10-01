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
      Product,
      Shipment,
      ShipmentLineItem,
      Subscription,
      Agency,
      Coupon,
      Price,
      GrowthRate,
      Review,
      Survey,
      FriendsFamilyProduct,
      FriendsFamilyOrder,
      FriendsFamilyOrderLineItem
    )

    //DataLoader.loadProducts
    //DataLoader.loadAdmin
    DataLoader.updateParentNoPets
    //DataLoader.createFriendsFamilyProducts
    DataLoader.updateShipmentShipStationId
    
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
