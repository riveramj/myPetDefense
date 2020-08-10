package com.mypetdefense.util

import net.liftweb.util._

object TitleCase {
  def apply(word: String): String = {
    word.toLowerCase.split(' ').map(_.capitalize).mkString(" ")
  }
}  

