package com.mypetdefense.snippet
package admin

import net.liftweb.util._
import Helpers._
import com.mypetdefense.model._
import com.mypetdefense.service.ValidationService._
import net.liftweb.common._
import net.liftweb.http._
import js.JsCmds._
import net.liftweb.http.SHtml.{ajaxInvoke, text}
import net.liftweb.http.js.JsCmd

import scala.xml.{Elem, NodeSeq}

object ProductSchedules extends Loggable {
  import com.mypetdefense.util.Paths._
  import net.liftweb.sitemap._

  val menu: Menu.Menuable = Menu.i("Product Schedules") / "admin" / "product-schedules" >>
    mpdAdmin >>
    loggedIn
}

class ProductSchedules extends Loggable {
  val coupons: List[Coupon] = Coupon.findAll()
  val dentalProducts = List(Product.dentalPowder, Product.dentalPowderSmall, Product.dentalPowderLarge).flatten
  val supplements: List[Product] = Product.supplements.filter(!dentalProducts.contains(_))
  val productSchedules: List[ProductSchedule] = ProductSchedule.findAll().sortBy(_.startDate.get)
  val startDateFormat = new java.text.SimpleDateFormat("M/d/y")

  var startDate = ""
  var firstBox = false
  var chosenSupplement1: Box[Product] = Empty
  var chosenSupplement2: Box[Product] = Empty
  var chosenSupplement3: Box[Product] = Empty

  def supplement1Dropdown: Elem = {
    SHtml.selectObj(
      supplements.map(supplement => (supplement, supplement.name.get)),
      chosenSupplement1,
      (supplement: Product) => chosenSupplement1 = Full(supplement)
    )
  }

  def supplement2Dropdown: Elem = {
    SHtml.selectObj(
      supplements.map(supplement => (supplement, supplement.name.get)),
      chosenSupplement2,
      (supplement: Product) => chosenSupplement2 = Full(supplement)
    )
  }

  def supplement3Dropdown: Elem = {
    SHtml.selectObj(
      supplements.map(supplement => (supplement, supplement.name.get)),
      chosenSupplement3,
      (supplement: Product) => chosenSupplement3 = Full(supplement)
    )
  }

  def createSchedule: JsCmd = {
    val validateFields = List(
      checkEmpty(startDate, "#start-date"),
      validDate(startDate, startDateFormat, "#start-date")
    ).flatten

    val selectedSupplements = List(chosenSupplement1, chosenSupplement2, chosenSupplement3, Product.dentalPowder).flatten

    if (validateFields.isEmpty) {
      ProductSchedule.createNew(startDateFormat.parse(startDate), selectedSupplements, firstBox)

      S.redirectTo(ProductSchedules.menu.loc.calcDefaultHref)
    } else {
      validateFields.foldLeft(Noop)(_ & _)
    }
  }

  def deleteSchedule(productSchedule: ProductSchedule)(): Alert = {
    productSchedule.scheduledItems.map(_.delete_!)
    productSchedule.delete_!

    S.redirectTo(ProductSchedules.menu.loc.calcDefaultHref)
  }

  def render: NodeSeq => NodeSeq = {
    SHtml.makeFormsAjax andThen
    ".product-schedules [class+]" #> "current" &
    "#start-date" #> text(startDate, startDate = _) &
    "#first-box" #> SHtml.checkbox(firstBox, firstBox = _) &
    ".choose-supplement #supplement-1" #> supplement1Dropdown &
    ".choose-supplement #supplement-2" #> supplement2Dropdown &
    ".choose-supplement #supplement-3" #> supplement3Dropdown &
    "#create-item" #> SHtml.ajaxSubmit("Create Schedule", () => createSchedule) &
    ".schedule" #> productSchedules.map { schedule =>
      val supplements = schedule.scheduledItems.toList.flatMap(_.product.obj).filter(!dentalProducts.contains(_))
      ".start-date *" #> startDateFormat.format(schedule.startDate.get) &
      ".first-box *" #> {
        if (schedule.firstBox.get)
          "Yes"
        else
          "No"
      } &
      ".supplement-name *" #> supplements.map(_.name.get) &
      ".status *" #> schedule.scheduleStatus.get.toString &
      ".actions .delete [onclick]" #> Confirm(
        s"Delete ${schedule.startDate.toString()}?",
        ajaxInvoke(deleteSchedule(schedule))
      )
    }
  }
}
