package com.mypetdefense.util

object TitleCase {
  def apply(word: String): String = {
    word.toLowerCase.split(' ').map(_.capitalize).mkString(" ")
  }
}
