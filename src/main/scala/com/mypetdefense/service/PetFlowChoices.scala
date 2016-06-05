package com.mypetdefense.service

import net.liftweb.sitemap.Menu
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.util.ClearClearable
import net.liftweb.http._
import net.liftweb.mapper.By

import com.mypetdefense.snippet.PetChoice._
import com.mypetdefense.snippet.CatSize._
import com.mypetdefense.model._

object PetFlowChoices extends Loggable {

  object petChoice extends SessionVar[Box[AnimalType.Value]](Empty)
  object petSize extends SessionVar[Box[AnimalSize.Value]](Empty)
  object petProduct extends SessionVar[Box[Product]](Empty)
}
