package com.mypetdefense.typeclasses

import net.liftweb.common.Box

import scala.language.higherKinds

trait MaybeLike[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def foreach[A, U](fa: F[A])(f: A => U): Unit
}

object MaybeLike {
  def apply[F[_]](implicit maybeLike: MaybeLike[F]): MaybeLike[F] = maybeLike

  implicit class Syntax[F[_]: MaybeLike, A](fa: F[A]) {
    def map[B](f: A => B): F[B]     = MaybeLike[F].map(fa)(f)
    def foreach[U](f: A => U): Unit = MaybeLike[F].foreach(fa)(f)
  }

  implicit val optionIsMaybeLike: MaybeLike[Option] = new MaybeLike[Option] {
    override def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt.map(f)
    override def foreach[A, U](opt: Option[A])(f: A => U): Unit  = opt.foreach(f)
  }

  implicit val boxIsMaybeLike: MaybeLike[Box] = new MaybeLike[Box] {
    override def map[A, B](box: Box[A])(f: A => B): Box[B]   = box.map(f)
    override def foreach[A, U](box: Box[A])(f: A => U): Unit = box.foreach(f)
  }
}
