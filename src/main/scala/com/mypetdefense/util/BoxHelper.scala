package com.mypetdefense.util

import net.liftweb.common.{Box, Empty, Full}

object BoxHelper {

  implicit class BoxCompanionOps(val box: Box.type) extends AnyVal {
    def nullable[T](value: T): Box[T] = if (value == null) Empty else Full(value)
  }

}
