package com.mypetdefense.util

import net.liftweb.sitemap._
import com.mypetdefense.snippet._

object Paths {

  val index = Menu.i("index") / "index"

  def siteMap = SiteMap(
    index,
    PetChoice.menu,
    DogSize.menu,
    CatSize.menu,
    DogProduct.menu,
    CatProduct.menu,
    Checkout.menu,
    Dashboard.menu,
    Parents.menu,
    Leads.menu,
    Agents.menu
  )
}
