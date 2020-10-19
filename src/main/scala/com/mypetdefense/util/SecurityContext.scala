package com.mypetdefense.util

import com.mypetdefense.model._
import com.mypetdefense.snippet.admin._
import com.mypetdefense.snippet.agency._
import com.mypetdefense.snippet.customer.{AccountOverview, UpgradeAccount}
import com.mypetdefense.snippet.login.Login
import com.mypetdefense.snippet.petland._
import net.liftweb.common._
import net.liftweb.http._

object SecurityContext extends Loggable {

  def loginRedirectUser(user: User): Nothing = {
    if (loggedIn_?) {
      logCurrentUserOut()
    }

    logIn(user)

    val possibleRedirect = Paths.intendedPath.is map { redirectLocation =>
      logger.info(s"Redirecting user to ${redirectLocation}.")
      S.redirectTo(redirectLocation)
    }

    val seenPrompt = user.subscription.obj.map(_.promptedUpgrade.get).openOr(false)
    val upgraded = user.subscription.obj
      .map(_.subscriptionBoxes.toList.map(_.subscriptionItems.toList.nonEmpty))
      .openOr(Nil)
      .foldLeft(true)(_ && _)

    user.userType.get match {
      case UserType.Admin =>
        possibleRedirect.openOr {
          logger.info("Redirecting user to admin redirect.")
          S.redirectTo(adminRedirect(user))
        }

      case UserType.Parent if seenPrompt || upgraded =>
        possibleRedirect.openOr {
          logger.info("Redirecting user to AccountOverview.")
          S.redirectTo(AccountOverview.menu.loc.calcDefaultHref)
        }

      case UserType.Parent =>
        possibleRedirect.openOr {
          logger.info("Redirecting user to UpgradeAccount.")
          S.redirectTo(UpgradeAccount.menu.loc.calcDefaultHref)
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

  def logIn(user: User): Unit = {
    loggedInUserId(Full(user.userId.get))
    loggedInUserId.is

    loggedInUser(Full(user))
    loggedInUser.is

    logger.info(s"Logged user in [ ${loggedInUser.is.map(_.email.get).openOr("")} ]")
  }

  def adminRedirect(user: User): String = {
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

  def logCurrentUserOut(): Box[User] = {
    loggedInUserId(Empty)
    loggedInUser(Empty)
  }

  def loggedIn_? : Boolean = {
    currentUser.isDefined
  }

  def currentUserId: Long = loggedInUserId.is.openOr(0)

  def agent_? : Boolean = {
    currentUser.map(_.userType == UserType.Agent) openOr false
  }

  def parent_? : Boolean = {
    currentUser.map(_.userType == UserType.Parent) openOr false
  }

  def currentUser: Box[User] = loggedInUser.is

  def mpdAdmin_? : Boolean = {
    (admin_? && currentUser
      .map(_.agency.obj.map(_.name.get == "My Pet Defense"))
      .flatten
      .openOr(false))
  }

  def admin_? : Boolean = {
    currentUser.map(_.userType == UserType.Admin) openOr false
  }

  private object loggedInUserId extends SessionVar[Box[Long]](Empty)

  private object loggedInUser extends SessionVar[Box[User]](Empty)
}
