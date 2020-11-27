package com.mypetdefense.util

import com.mypetdefense.typeclasses.MaybeLike
import com.mypetdefense.typeclasses.MaybeLike.Syntax

import scala.language.higherKinds

object StripeHelper {

  implicit class ApiRequestParamsBuilderOps[B](val builder: B) extends AnyVal {
    def whenDefined[F[_]: MaybeLike, T](maybeValue: F[T])(setValue: B => T => B): B = {
      maybeValue foreach setValue(builder)
      builder
    }

    def whenDefinedC[F[_]: MaybeLike, T, R](
        maybeValue: F[T]
    )(setValue: B => R => B)(implicit ev: T => R): B =
      whenDefined(maybeValue.map(ev))(setValue)

    def when(cond: Boolean)(thenPart: B => Unit)(elsePart: B => Unit): B = {
      if (cond) thenPart(builder) else elsePart(builder)
      builder
    }
  }

}
