package com.fleaTick.util

import net.liftweb.sitemap._
import com.fleaTick.snippet._

object Paths {

  val index = Menu.i("index") / "index"
  
  def siteMap = SiteMap(
    index,
    PetChoice.menu,
    PetSize.menu,
    PetProduct.menu
  )
}
