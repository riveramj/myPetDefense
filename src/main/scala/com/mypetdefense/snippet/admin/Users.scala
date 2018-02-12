package com.mypetdefense.snippet
package admin

import net.liftweb._
  import sitemap.Menu
  import http.SHtml._
  import http._
  import js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.mapper.By

import java.text.SimpleDateFormat
import java.util.Date
import java.time.{LocalDate, ZoneId}

import com.mypetdefense.model._
import com.mypetdefense.util.ClearNodesIf
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.actor._

object Users extends Loggable {
  import net.liftweb.sitemap._
    import Loc._
  import com.mypetdefense.util.Paths._

  val menu = Menu.i("Users") / "admin" / "users" >>
    adminUser >>
    loggedIn
}

class Users extends Loggable {
  val users = User.findAll()
  val allAgencies = Agency.findAll()

  var firstName = ""
  var lastName = ""
  var email = ""
  var userType: Box[UserType.Value] = Full(UserType.Agent)
  var chosenAgency: Box[Agency] = Empty
  var admin_? = false
  
  def agencyDropdown = {
    SHtml.selectObj(
        allAgencies.map(agency => (agency, agency.name.get)),
        chosenAgency,
        (agency: Agency) => chosenAgency = Full(agency)
      )
  }

  def createUser = {
    val validateFields = List(
      checkEmail(email, "#email"),
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name")
    ).flatten

    if(validateFields.isEmpty) {
      val newUser = User.createNewPendingUser(
        firstName,
        lastName,
        email,
        UserType.Admin,
        chosenAgency
      )
      
      EmailActor ! SendNewUserEmail(newUser)

      S.redirectTo(Users.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def userTypeRadio(renderer: IdMemoizeTransform) = {
    ajaxRadio(
      List(UserType.Agent, UserType.Admin), 
      userType, 
      (userSelected: UserType.Value) => {
        userType = Full(userSelected)
        admin_? = {
          if (userSelected == UserType.Admin)
            true
          else
            false
        }
        renderer.setHtml
      }
    ).toForm
  }

  def deleteUser(user: User)() = {
    user.userType match {
      case parent if (parent == UserType.Parent) =>
        Noop
      case _ =>
        if (user.delete_!)
          S.redirectTo(Users.menu.loc.calcDefaultHref)
        else
          Alert("An error has occured. Please try again.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".users [class+]" #> "current" &
    ".create" #> idMemoize { renderer =>
      "#first-name" #> ajaxText(firstName, firstName = _) &
      "#last-name" #> ajaxText(lastName, lastName = _) &
      "#email" #> ajaxText(email, userEmail => email = userEmail.trim) &
      "#user-type-select" #> userTypeRadio(renderer) &
      "#agency-container" #> ClearNodesIf(admin_?) &
      "#agency-container #agency-select" #> agencyDropdown &
      "#create-item" #> SHtml.ajaxSubmit("Create User", () => createUser)
    } &
    ".user" #> users.map { user =>
      ".name *" #> user.name &
      ".email *" #> user.email &
      ".type *" #> user.userType &
      ".agency *" #> user.agency.obj.map(_.name.get) &
      ".actions .delete" #> ClearNodesIf(user.userType == UserType.Parent) &
      ".actions .delete [onclick]" #> Confirm(s"Delete ${user.name}?",
        ajaxInvoke(deleteUser(user) _)
      )
    }
  }
}


