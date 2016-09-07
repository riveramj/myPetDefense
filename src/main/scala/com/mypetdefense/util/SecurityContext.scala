package com.mypetdefense.util

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._

import com.mypetdefense.model._

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
}
