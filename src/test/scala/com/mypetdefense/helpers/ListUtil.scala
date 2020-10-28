package com.mypetdefense.helpers

object ListUtil {

  def splitInHalf[T](in: List[T]): (List[T], List[T]) = in.splitAt(in.size / 2)

  implicit class ListOps[T](val v: List[T]) extends AnyVal {
    def splitInTwo: (List[T], List[T]) = splitInHalf(v)
  }

}
