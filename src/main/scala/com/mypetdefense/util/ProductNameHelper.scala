package com.mypetdefense.util

object ProductNameHelper {

  def sanitizeFleaTickNames(productsNameAndSize: List[String]): List[String] =
    productsNameAndSize.map {
      case product if product.contains("5-22") =>
        "ZoGuard Plus for Dogs 05-22 lbs"
      case product if product.contains("3-10") =>
        "Adventure Plus for Dogs, 3-10 lbs"
      case product if product.contains("5-15") =>
        "ShieldTec Plus for Dogs, 05-15 lbs"
      case unhandledName =>
        unhandledName
    }

}
