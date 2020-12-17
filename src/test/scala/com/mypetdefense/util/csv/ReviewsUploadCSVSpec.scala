package com.mypetdefense.util.csv

import com.mypetdefense.model.{FleaTick, Review}
import com.mypetdefense.util.DateHelper.makeDate
import net.liftweb.common.Full
import net.liftweb.mapper.By

class ReviewsUploadCSVSpec extends GenericCSVParserSpec[Review] {

  override val parser = ReviewsUploadCSV

  it should "properly parse ReviewsUploadCSV" in {
    val csv =
      """Title,Body,Rating,Author,Date,Product
        |"Some title","Some body",4.2,"John Doe","Dec 15, 2020",ZoGuard Plus for Dogs""".stripMargin

    csv checkFirst { review =>
      review.title.get mustBe "Some title"
      review.body.get mustBe "Some body"
      review.rating.get mustBe 4.2
      review.author.get mustBe "John Doe"
      review.date.get mustBe makeDate(2020, 12, 15)
      FleaTick.find(By(FleaTick.id, review.fleaTick.get)).map(_.name) mustBe Full(
        "ZoGuard Plus for Dogs"
      )
    }
  }
}
