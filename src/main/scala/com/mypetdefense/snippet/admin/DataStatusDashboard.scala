package com.mypetdefense.snippet.admin


import com.mypetdefense.model._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import java.text.NumberFormat
import scala.collection.immutable

object DataStatusDashboard extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Data Status Dashboard") / "admin" / "data-status-dashboard" >>
    mpdAdmin >>
    loggedIn
}

class DataStatusDashboard extends Loggable {
  val parentsCount: Long = User.count(By(User.userType, UserType.Parent))
  val parentsWithAgencyCount: Long = User.count(By(User.userType, UserType.Parent), NotNullRef(User.referer))
  val agenciesCustomersCount: Int = Agency.findAll().flatMap(_.customers.toList).size

  val activeUsers: immutable.Seq[User] = User.findAll(By(User.userType, UserType.Parent),By(User.status, Status.Active))
  val activeUsersSubscriptionCount: Int = activeUsers
    .flatMap(_.subscription.obj.toList)
    .count(_.status != Status.Cancelled)

  val nonCancelledSubscription: Seq[Subscription] = Subscription.findAll(NotBy(Subscription.status, Status.Cancelled))
  val nonCancelledSubscriptionUserCount: Int = nonCancelledSubscription
    .flatMap(_.user.obj.toList)
    .count(_.status != Status.Cancelled)

  val cancelledUsers: immutable.Seq[User] = User.findAll(By(User.userType, UserType.Parent), By(User.status, Status.Cancelled))
  val cancelledUsersSubscriptionCount: Int = cancelledUsers
    .flatMap(_.subscription.obj.toList)
    .count(_.status == Status.Cancelled)

  val cancelledSubscription: Seq[Subscription] = Subscription.findAll(By(Subscription.status, Status.Cancelled))
  val cancelledSubscriptionUserCount: Int = cancelledSubscription
    .flatMap(_.user.obj.toList)
    .count(_.status == Status.Cancelled)

  val activeSubscription = Subscription.findAll(
    ByList(Subscription.status, List(Status.Active, Status.Paused))
  )

  val activeSubscriptionHasBox = activeSubscription
    .count(_.subscriptionBoxes.map(_.status.get).contains(Status.Active))

  val activeSubscriptionBox = SubscriptionBox.findAll(By(SubscriptionBox.status, Status.Active))

  val activeBoxPetCount = activeSubscriptionBox.count(_.pet.obj.map(_.status.get).contains(Status.Active))

  val activeSubscriptionBoxHasSubscription = activeSubscriptionBox.flatMap(_.subscription.obj)

  val activePetsActiveSubsCount = activeSubscription.count(_.getPets.map(_.status.get).contains(Status.Active))

  val activePets = Pet.findAll(By(Pet.status, Status.Active))
  val activePetBoxCount = activePets.count(_.box.obj.map(_.status.get).contains(Status.Active))

  def formatPercentage(num: Long, dem: Long): String =
    NumberFormat.getPercentInstance.format(num/dem.toDouble)

  def render: CssBindFunc = {
    ".data-status-dashboard [class+]" #> "current" &
    ".customers-and-agencies"#> {
      ".customers-have-agency *" #> formatPercentage(parentsWithAgencyCount, parentsCount) &
      ".agencies-have-customers *" #> formatPercentage(agenciesCustomersCount, parentsCount)
    } &
    ".users-subscription-active"#> {
      ".active-parents-subscription *" #> formatPercentage(activeUsersSubscriptionCount, activeUsers.size) &
      ".non-cancelled-subscription-user *" #> formatPercentage(nonCancelledSubscriptionUserCount, nonCancelledSubscription.size)
    } &
    ".users-subscription-cancelled"#> {
      ".cancelled-parents-subscription *" #> formatPercentage(cancelledUsersSubscriptionCount, cancelledUsers.size) &
      ".cancelled-subscription-user *" #> formatPercentage(cancelledSubscriptionUserCount, cancelledSubscription.size)
    } &
    ".active-subscription-pets" #> {
      ".active-subscription-has-pets *" #> formatPercentage(activePetsActiveSubsCount, activeSubscription.size)
    } &
    ".subscription-box-subscription" #> {
      ".active-subscription-has-box *" #> formatPercentage(activeSubscriptionHasBox, activeSubscription.size) &
      ".active-subscription-box-subscription *" #> formatPercentage(activeSubscriptionBoxHasSubscription.size, activeSubscriptionBox.size)
    } &
    ".pets-subscription-box" #> {
      ".active-box-has-pet *" #> formatPercentage(activeBoxPetCount, activeSubscriptionBox.size) &
      ".active-pet-has-box *" #> formatPercentage(activePetBoxCount, activePets.size)
    }
  }
}
