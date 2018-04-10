package com.mypetdefense.util

import net.liftweb.util._

object TitleCase {
  def apply(word: String) = {
    word.toLowerCase.split(' ').map(_.capitalize).mkString(" ")
  }
}  

