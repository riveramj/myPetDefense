package com.mypetdefense.helpers

import org.apache.shiro.crypto.SecureRandomNumberGenerator

object Random {

  private val rng = new SecureRandomNumberGenerator()

  def genPosLong: Long = scala.math.abs(scala.util.Random.nextLong)

  def generateMoneyString: String = {
    f"${genPosLong.toDouble}%2.2f"
  }

  def generateString: String = {
    rng.nextBytes(32).toBase64
  }

}