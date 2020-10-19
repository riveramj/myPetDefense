package com.mypetdefense.util

import net.liftweb.common.Loggable
import org.apache.shiro.crypto.SecureRandomNumberGenerator

object RandomIdGenerator extends Loggable {
  private val rng = new SecureRandomNumberGenerator()

  def generateLongId: Long = {
    scala.math.abs(scala.util.Random.nextLong)
  }

  def generateStringId: String = {
    rng.nextBytes(32).toBase64
  }
}
