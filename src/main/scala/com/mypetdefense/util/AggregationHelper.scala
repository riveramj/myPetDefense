package com.mypetdefense.util

object AggregationHelper {

  def combineSimilarItems[T, S](
      input: List[T]
  )(similarity: T => S, combine: (T, T) => T): List[T] = {
    val grouped = input.groupBy(similarity).mapValues(_.reduceLeft(combine))
    val order   = input.map(similarity).distinct
    order.map(grouped)
  }

}
