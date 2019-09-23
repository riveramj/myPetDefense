package com.mypetdefense.util

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._

import com.mypetdefense.model._
import com.mypetdefense.snippet._
import com.mypetdefense.snippet.admin._
import com.mypetdefense.snippet.petland._
import com.mypetdefense.snippet.agency._

object SecurityContext extends Loggable {

  private object loggedInUserId extends SessionVar[Box[Long]](Empty)
  private object loggedInUser extends SessionVar[Box[User]](Empty)

  def logIn(user: User) = {
    loggedInUserId(Full(user.userId.get))
    loggedInUserId.is

    loggedInUser(Full(user))
    loggedInUser.is

    logger.info(s"Logged user in [ ${loggedInUser.is.map(_.email.get).openOr("")} ]")
  }

  def loginRedirectUser(user: User) = {
    if (loggedIn_?) {
      logCurrentUserOut()
    }

    logIn(user)

    val possibleRedirect = Paths.intendedPath.is map { redirectLocation =>
      logger.info(s"Redirecting user to ${redirectLocation}.")
      S.redirectTo(redirectLocation)
    } 

    user.userType.get match {
      case UserType.Admin =>
        possibleRedirect.openOr {
          logger.info("Redirecting user to admin redirect.")
          S.redirectTo(adminRedirect(user))
        }

      case UserType.Parent  =>
        possibleRedirect.openOr {
          logger.info("Redirecting user to AccountOverview.")
          S.redirectTo(AccountOverview.menu.loc.calcDefaultHref)
        }

      case UserType.Agent =>
        possibleRedirect.openOr {
          logger.info("Redirecting user to New Order Form.")
          S.redirectTo(NewOrder.menu.loc.calcDefaultHref)
        }

      case other =>
        logger.info(s"Not valid user type: $other")
        S.redirectTo(Login.menu.loc.calcDefaultHref)
    }
  }

  def adminRedirect(user: User) = {
    println(user)

    val agencyName = (for {
      agency <- user.agency.obj
      } yield {
        agency.name.get
      })

    if (agencyName == "My Pet Defense") {
      ShipmentDashboard.menu.loc.calcDefaultHref
    } else {
      AgencyOverview.menu.loc.calcDefaultHref
    }
  }

  def logCurrentUserOut() = {
    loggedInUserId(Empty)
    loggedInUser(Empty)
  }

  def currentUser: Box[User] = loggedInUser.is
  def currentUserId = loggedInUserId.is.openOr(0)

  def loggedIn_? : Boolean = {
    currentUser.isDefined
  }

  def agent_? : Boolean = {
    currentUser.map(_.userType == UserType.Agent) openOr false
  }

  def parent_? : Boolean = {
    currentUser.map(_.userType == UserType.Parent) openOr false
  }

  def admin_? : Boolean = {
    currentUser.map(_.userType == UserType.Admin) openOr false
  }

  def mpdAdmin_? : Boolean = {
    (admin_? && currentUser.map(_.agency.obj.map(_.name.get == "My Pet Defense")).flatten.openOr(false))
  }
}
