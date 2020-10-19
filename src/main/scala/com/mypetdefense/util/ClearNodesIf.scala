package com.mypetdefense.util

import net.liftweb.util._

import scala.xml.NodeSeq

object ClearNodesIf {
  def apply(test: => Boolean): NodeSeq => NodeSeq = {
    if (test)
      ClearNodes
    else
      PassThru
  }
}
