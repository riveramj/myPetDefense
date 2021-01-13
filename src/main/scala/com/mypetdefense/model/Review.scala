package com.mypetdefense.model

import com.mypetdefense.util.RandomIdGenerator._
import net.liftweb.common.Box
import net.liftweb.mapper._

class Review extends LongKeyedMapper[Review] with IdPK with OneToMany[Long, Review] {
  def getSingleton: KeyedMetaMapper[Long, Review] = Review

  object reviewId extends MappedLong(this) {
    override def dbIndexed_? = true
  }
  object title     extends MappedString(this, 100)
  object body      extends MappedString(this, 1000)
  object rating    extends MappedDouble(this)
  object author    extends MappedString(this, 100)
  object date      extends MappedZonedDateTime(this)
  object fleaTick  extends MappedLongForeignKey(this, FleaTick)
  object createdAt extends MappedZonedDateTime(this, useNowAsDefault = true)
}

object Review extends Review with LongKeyedMetaMapper[Review] {
  def createReview(
      title: String,
      body: String,
      rating: Double,
      author: String,
      fleaTick: Box[FleaTick]
  ): Review = {
    Review.create
      .reviewId(generateLongId)
      .title(title)
      .body(body)
      .rating(rating)
      .author(author)
      .fleaTick(fleaTick)
      .saveMe
  }
}
