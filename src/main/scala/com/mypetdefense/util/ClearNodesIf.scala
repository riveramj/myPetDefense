package com.mypetdefense.util

import net.liftweb.util._

object ClearNodesIf {
  def apply(test: =>Boolean) = {
    if (test)
      ClearNodes
    else
      PassThru
  }
}  
