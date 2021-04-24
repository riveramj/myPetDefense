package com.mypetdefense.service

import java.text.SimpleDateFormat

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.LiftTest
import com.mypetdefense.model.{Coupon, Subscription, User, UserType}
import com.mypetdefense.util.DateHelper.makeDate
import net.liftweb.common.{Empty, Full}

class ValidationServiceSpec extends LiftTest {
  import ValidationService._
  import ValidationServiceSpec._

  "checkEmpty" should "pass any non-empty string" in {
    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr) { (fieldValue, fieldId) =>
      checkEmpty(fieldValue, fieldId) mustBe Empty
    }
  }

  it should "reject empty string" in {
    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkEmpty(fieldValue = "", fieldId) mustBe
        Full(ValidationError(fieldId, "Required."))
    }
  }

  it should "pass any non-empty box" in {
    forAllNoShrink(genNonEmptyBoxStr, genNonEmptyAlphaStr) { (fieldValue, fieldId) =>
      checkEmpty(fieldValue, fieldId) mustBe Empty
    }
  }

  it should "reject empty box" in {
    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkEmpty(fieldValue = Empty, fieldId) mustBe
        Full(ValidationError(fieldId, "Required."))
    }
  }

  "checkDuplicateCoupon" should "allow creating coupons when there are none" in {
    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr) { (coupon, fieldId) =>
      checkDuplicateCoupon(coupon, fieldId) mustBe Empty
    }
  }

  it should "allow creating coupons when there are no duplicates" in {
    Coupon.createNewCoupon(couponCode = "existing", agency = Empty)

    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr) { (coupon, fieldId) =>
      whenever(coupon != "existing") {
        checkDuplicateCoupon(coupon, fieldId) mustBe Empty
      }
    }
  }

  it should "deny creating coupons when there are duplicates" in {
    Coupon.createNewCoupon(couponCode = "existing", agency = Empty)

    checkDuplicateCoupon(coupon = "existing", errorId = "fieldId") mustBe
      Full(ValidationError("fieldId", "Code already exists."))
  }

  "validEmailFormat" should "accept valid emails" in {
    forAllNoShrink(genEmailStr, genNonEmptyAlphaStr) { (email, fieldId) =>
      validEmailFormat(email, fieldId) mustBe Empty
    }
  }

  it should "reject invalid emails" in {
    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr) { (email, fieldId) =>
      validEmailFormat(email, fieldId) mustBe
        Full(ValidationError(fieldId, "Not valid email address."))
    }
  }

  it should "reject the empty string" in {
    validEmailFormat(email = "", errorId = "fieldId") mustBe
      Full(ValidationError("fieldId", "Required."))
  }

  "validNumber" should "accept valid numbers" in {
    forAllNoShrink(genSignedNumStr, genNonEmptyAlphaStr) { (number, fieldId) =>
      validNumber(number, fieldId) mustBe Empty
    }
  }

  it should "reject random strings" in {
    forAllNoShrink(genAlphaStr, genNonEmptyAlphaStr) { (number, fieldId) =>
      validNumber(number, fieldId) mustBe
        Full(ValidationError(fieldId, "Not a valid number."))
    }
  }

  "checkEmail" should "accept valid emails" in {
    forAllNoShrink(genEmailStr, genNonEmptyAlphaStr, genBool) { (email, fieldId, isSignup) =>
      checkEmail(email, fieldId, isSignup) mustBe Empty
    }
  }

  it should "accept when user has no subscriptions on signup" in {
    newUserWithEmail()

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkEmail(email = "john.doe@example.com", fieldId, signup = true) mustBe Empty
    }
  }

  it should "reject when user has subscriptions on signup" in {
    newSubscription(newUserWithEmail())

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkEmail(email = "john.doe@example.com", fieldId, signup = true) mustBe
        Full(ValidationError(fieldId, "Email already exists."))
    }
  }

  it should "reject when user has no subscriptions not on signup" in {
    newUserWithEmail()

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkEmail(email = "john.doe@example.com", fieldId) mustBe
        Full(ValidationError(fieldId, "Email already exists."))
    }
  }

  it should "reject when user has subscriptions not on signup" in {
    newSubscription(newUserWithEmail())

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkEmail(email = "john.doe@example.com", fieldId) mustBe
        Full(ValidationError(fieldId, "Email already exists."))
    }
  }

  it should "reject random strings" in {
    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr, genBool) {
      (email, fieldId, isSignup) =>
        checkEmail(email, fieldId, isSignup) mustBe
          Full(ValidationError(fieldId, "Not valid email address."))
    }
  }

  it should "reject empty string" in {
    forAllNoShrink(genNonEmptyAlphaStr, genBool) { (fieldId, isSignup) =>
      checkEmail(email = "", fieldId, isSignup) mustBe
        Full(ValidationError(fieldId, "Required."))
    }
  }

  "checkFacebookId" should "accept valid Facebook IDs" in {
    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr, genBool) { (fbId, fieldId, isSignup) =>
      checkFacebookId(fbId, fieldId, isSignup) mustBe Empty
    }
  }

  it should "accept when user has no subscriptions on signup" in {
    newUserWithFbId()

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkFacebookId(facebookId = "fb1234", fieldId, signup = true) mustBe Empty
    }
  }

  it should "reject when user has subscriptions on signup" in {
    newSubscription(newUserWithFbId())

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkFacebookId(facebookId = "fb1234", fieldId, signup = true) mustBe
        Full(ValidationError(fieldId, "Facebook account already exists."))
    }
  }

  it should "reject when user has no subscriptions not on signup" in {
    newUserWithFbId()

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkFacebookId(facebookId = "fb1234", fieldId) mustBe
        Full(ValidationError(fieldId, "Facebook account already exists."))
    }
  }

  it should "reject when user has subscriptions not on signup" in {
    newSubscription(newUserWithFbId())

    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkFacebookId(facebookId = "fb1234", fieldId) mustBe
        Full(ValidationError(fieldId, "Facebook account already exists."))
    }
  }

  it should "reject empty string" in {
    forAllNoShrink(genNonEmptyAlphaStr, genBool) { (fieldId, isSignup) =>
      checkFacebookId(facebookId = "", fieldId, isSignup) mustBe
        Full(ValidationError(fieldId, "Required."))
    }
  }

  "checkCouponValue" should "accept anything but zero" in {
    forAllNoShrink(genAlphaStr, genNonEmptyAlphaStr) { (coupon, fieldId) =>
      whenever(coupon.trim != "0") {
        checkCouponValue(coupon, fieldId) mustBe Empty
      }
    }
  }

  it should "reject zero" in {
    forAllNoShrink(genNonEmptyAlphaStr) { fieldId =>
      checkCouponValue(value = "  0    ", fieldId) mustBe
        Full(ValidationError(fieldId, "Cannot be 0."))
    }
  }

  "checkMonthPercentDollar" should "require at least one of the fields" in {
    checkMonthPercentDollar(
      ("", "monthId"),
      ("", "percentId"),
      ("", "dollarId")
    ) mustBe List(
      Full(ValidationError("monthId", "One of these is required.")),
      Full(ValidationError("percentId", "One of these is required.")),
      Full(ValidationError("dollarId", "One of these is required."))
    )
  }

  it should "guarantee that percent and dollar are mutually exclusive" in {
    forAllNoShrink(genNumStr, genNonEmptyAlphaStr, genNonEmptyAlphaStr) {
      (months, percent, dollar) =>
        checkMonthPercentDollar(
          (months, "monthId"),
          (percent, "percentId"),
          (dollar, "dollarId")
        ) mustBe List(
          Full(ValidationError("percentId", "One of these must be empty.")),
          Full(ValidationError("dollarId", "One of these must be empty."))
        )
    }
  }

  "checkNonZero" should "accept any positive number as first quantity" in {
    forAllNoShrink(genPosInt, genNonPosInt, genNonEmptyAlphaStr, genNonEmptyAlphaStr) {
      (quantity1, quantity2, field1, field2) =>
        checkNonZero(quantity1, quantity2, field1, field2) mustBe List(Empty)
    }
  }

  it should "accept any positive number as second quantity" in {
    forAllNoShrink(genNonPosInt, genPosInt, genNonEmptyAlphaStr, genNonEmptyAlphaStr) {
      (quantity1, quantity2, field1, field2) =>
        checkNonZero(quantity1, quantity2, field1, field2) mustBe List(Empty)
    }
  }

  it should "reject when both quantities are non-positive" in {
    forAllNoShrink(genNonPosInt, genNonPosInt, genNonEmptyAlphaStr, genNonEmptyAlphaStr) {
      (quantity1, quantity2, field1, field2) =>
        checkNonZero(quantity1, quantity2, field1, field2) mustBe List(
          Full(ValidationError(field1, "Need at least one item ordered.")),
          Full(ValidationError(field2, "Need at least one item ordered."))
        )
    }
  }

  "validDate" should "accept valid date" in {
    validDate("11/30/2020", dateFormat, "fieldId") mustBe Empty
  }

  it should "accept empty string" in {
    validDate("", dateFormat, "fieldId") mustBe Empty
  }

  it should "reject random strings" in {
    forAllNoShrink(genNonEmptyAlphaStr, genNonEmptyAlphaStr) { (date, fieldId) =>
      validDate(date, dateFormat, fieldId) mustBe
        Full(ValidationError(fieldId, "Not a valid date format."))
    }
  }

}

object ValidationServiceSpec {
  val dateFormat = new SimpleDateFormat("M/d/y")

  def newUserWithEmail(): User =
    User.createNewUser(
      firstName = "John",
      lastName = "Doe",
      stripeId = "cus_1234",
      email = "john.doe@example.com",
      password = "secret",
      phone = "123456789",
      coupon = Empty,
      referer = Empty,
      agency = Empty,
      UserType.Agent,
      ""
    )

  def newUserWithFbId(): User =
    newUserWithEmail()
      .facebookId("fb1234")
      .saveMe()

  def newSubscription(user: User): Subscription = {
    val sub = Subscription.createNewSubscription(
      Full(user),
      stripeSubscriptionId = "sub_1234",
      startDate = makeDate(2020, 1, 1),
      nextShipDate = makeDate(2020, 2, 1)
    )

    user.subscription(sub).saveMe()

    sub
  }
}
