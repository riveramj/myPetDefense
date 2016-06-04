package com.mypetdefense.util

import net.liftweb.sitemap._
import com.mypetdefense.snippet._

object Paths {

  val index = Menu.i("index") / "index"

  val catSize = Menu.i("Choose Cat Size") / "cat-size"
  
  def siteMap = SiteMap(
    index,
    catSize,
    PetChoice.menu,
    PetSize.menu,
    PetProduct.menu,
    Checkout.menu,
    Dashboard.menu,
    Parents.menu,
    Leads.menu,
    Agents.menu
  )
}
