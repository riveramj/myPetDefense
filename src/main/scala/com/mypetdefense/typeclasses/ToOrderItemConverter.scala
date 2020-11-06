package com.mypetdefense.typeclasses

import com.mypetdefense.shipstation.OrderItem

trait ToOrderItemConverter[T] {
  def toOrderItem(input: T, index: Int): OrderItem
}

object ToOrderItemConverter {
  implicit class ToOrderItemConverterOps[T](val v: T) extends AnyVal {
    def toOrderItem(index: Int)(implicit converter: ToOrderItemConverter[T]): OrderItem =
      converter.toOrderItem(v, index)
  }
}
