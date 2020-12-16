package com.mypetdefense.snippet
package admin

import com.mypetdefense.actor._
import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import com.mypetdefense.util.ClearNodesIf
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper.By
import net.liftweb.util.Helpers._

import scala.xml.{Elem, NodeSeq}

object Users extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Users") / "admin" / "users" >>
    mpdAdmin >>
    loggedIn
}

class Users extends Loggable {
  val users: List[User] = User
    .findAll(
      By(User.status, Status.Active)
    )
    .filter(_.userType != UserType.Parent)
  val allAgencies: List[Agency] = Agency.findAll()

  var firstName                     = ""
  var lastName                      = ""
  var email                         = ""
  var userType: Box[UserType.Value] = Full(UserType.Agent)
  var chosenAgency: Box[Agency]     = Empty

  def agencyDropdown: Elem = {
    SHtml.selectObj(
      allAgencies.map(agency => (agency, agency.name.get)),
      chosenAgency,
      (agency: Agency) => chosenAgency = Full(agency)
    )
  }

  def createUser: JsCmd = {
    val validateFields = List(
      checkEmail(email, "#email"),
      checkEmpty(firstName, "#first-name"),
      checkEmpty(lastName, "#last-name")
    ).flatten

    if (validateFields.isEmpty) {
      val newUser = userType.map { selectedType =>
        User.createNewPendingUser(
          firstName,
          lastName,
          email,
          selectedType,
          chosenAgency,
          None,
          ""
        )
      }

      if (userType == Full(UserType.Admin))
        newUser.map(EmailActor ! SendNewAdminEmail(_))
      else if (userType == Full(UserType.Agent))
        newUser.map(EmailActor ! SendNewAgentEmail(_))
      else
        newUser.map(EmailActor ! SendNewUserEmail(_))

      S.redirectTo(Users.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def userTypeRadio(renderer: IdMemoizeTransform): NodeSeq = {
    ajaxRadio(
      List(UserType.Agent, UserType.Admin),
      userType,
      (userSelected: UserType.Value) => {
        userType = Full(userSelected)
        renderer.setHtml
      }
    ).toForm
  }

  def deleteUser(user: User)(): JsCmd = {
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

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
      ".users [class+]" #> "current" &
        ".create" #> idMemoize { renderer =>
          "#first-name" #> ajaxText(firstName, firstName = _) &
            "#last-name" #> ajaxText(lastName, lastName = _) &
            "#email" #> ajaxText(email, userEmail => email = userEmail.trim) &
            "#user-type-select" #> userTypeRadio(renderer) &
            "#agency-container #agency-select" #> agencyDropdown &
            "#create-item" #> SHtml.ajaxSubmit("Create User", () => createUser)
        } &
        ".user" #> users.map { user =>
          ".name *" #> user.name &
            ".email *" #> user.email &
            ".type *" #> user.userType &
            ".agency *" #> user.agency.obj.map(_.name.get) &
            ".actions .delete" #> ClearNodesIf(user.userType == UserType.Parent) &
            ".actions .delete [onclick]" #> Confirm(
              s"Delete ${user.name}?",
              ajaxInvoke(deleteUser(user) _)
            )
        }
  }
}
