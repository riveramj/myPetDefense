package com.mypetdefense.util

import net.liftweb.sitemap._
import com.mypetdefense.snippet._

object Paths {

  val index = Menu.i("index") / "index"
  
  def siteMap = SiteMap(
    index,
    PetChoice.menu,
    PetSize.menu,
    PetProduct.menu,
    Checkout.menu,
    Dashboard.menu
  )
}
