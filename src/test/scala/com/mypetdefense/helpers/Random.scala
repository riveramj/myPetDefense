package com.mypetdefense.helpers

import org.apache.shiro.crypto.SecureRandomNumberGenerator

object Random {

  private val rng = new SecureRandomNumberGenerator()

  def randomPosLong: Long = scala.math.abs(scala.util.Random.nextLong)

  def randomPosInt: Int = scala.math.abs(scala.util.Random.nextInt())

  def generateMoneyString: String = {
    f"${randomPosLong.toDouble}%2.2f"
  }

  def generateString: String = {
    rng.nextBytes(32).toBase64
  }

  def generateIntBetween(start: Int, end: Int): Int = {
    val r = new scala.util.Random
    start + r.nextInt((end - start) + 1)
  }

}
