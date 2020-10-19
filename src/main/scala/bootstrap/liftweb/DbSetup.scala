package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.http.LiftRules
import net.liftweb.mapper.DB
import net.liftweb.mapper.StandardDBVendor
import net.liftweb.util
import net.liftweb.util.Props

object DbSetup extends Loggable {

  def setup = {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(
          Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"),
          Props.get("db.password")
        )

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }
  }
}
