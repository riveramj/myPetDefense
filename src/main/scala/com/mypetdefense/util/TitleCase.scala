package com.mypetdefense.util

object TitleCase {
  def apply(word: String): String = {
    word.toLowerCase.split(' ').map(_.capitalize).mkString(" ")
  }
}

object SplitTitleCase {
  def apply(word: String): String =
    word.split("(?<!^)(?=[A-Z])").mkString(" ")
}