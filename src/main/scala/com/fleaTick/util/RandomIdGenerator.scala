package com.fleaTick.util

import org.apache.shiro.crypto.SecureRandomNumberGenerator
import net.liftweb.common.Loggable

object RandomIdGenerator extends Loggable {
  private val rng = new SecureRandomNumberGenerator()

  def generateLongId = {
    scala.math.abs(scala.util.Random.nextLong)
  }

  def generateStringId = {
    rng.nextBytes(32).toBase64
  }
}

