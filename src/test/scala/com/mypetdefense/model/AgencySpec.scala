package com.mypetdefense.model

import com.mypetdefense.generator.Generator._
import com.mypetdefense.helpers.DateUtil.{anyDayOfThisMonth, _}
import com.mypetdefense.helpers.GeneralDbUtils._
import com.mypetdefense.helpers._
import com.mypetdefense.helpers.db.AgencyDbUtils.createAgency
import net.liftweb.common.{Empty, Full}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class AgencySpec extends DBTest {
  it should "find all agency shipments" in {
    forAll(
      genShipmentChainData,
      genShipmentChainData
    ) {
      (agencyShipments, nonAgencyShipments) =>
        val mpdAgency = createAgency(mpdAgencyName)
        val forAgency = insertUserSubAndShipment(agencyShipments)

        forAgency.user.referer(mpdAgency).saveMe()

        insertUserSubAndShipment(nonAgencyShipments)

        val expectedShipments = forAgency.shipments.map(_.id.get)

        val actualData = mpdAgency.reload.findAllShipments.map(_.id.get)

        actualData should contain theSameElementsAs expectedShipments
        cleanUpSuccess()
    }
  }

  it should "find current month agency shipments" in {
    forAll(
      genShipmentChainData,
      genShipmentChainData,
      genShipmentChainData,
    ) {
      (agencyShipmentsThisMonth, agencyShipmentsNotThisMonth, nonAgencyShipmentsThisMonth ) =>
        val mpdAgency = createAgency(mpdAgencyName)

        val forAgencyThisMonth = insertUserSubAndShipment(agencyShipmentsThisMonth)
        val forAgencyLastMonth = insertUserSubAndShipment(agencyShipmentsNotThisMonth)

        forAgencyLastMonth.user.referer(mpdAgency).saveMe()
        forAgencyLastMonth.shipments.map(_.dateShipped(anyDayOfLastMonth.toDate).saveMe())

        insertUserSubAndShipment(nonAgencyShipmentsThisMonth)
          .shipments.map(_.dateShipped(anyDayOfThisMonth.toDate).saveMe())

        forAgencyThisMonth.user.referer(mpdAgency).saveMe()
        val expectedShipments = forAgencyThisMonth
          .shipments.map(_.dateShipped(anyDayOfThisMonth.toDate).saveMe().id.get)

        val actualData = mpdAgency.reload.currentMonthShipments.map(_.id.get)

        actualData should contain theSameElementsAs expectedShipments
        cleanUpSuccess()
    }
  }

  it should "get all customers for HQ" in {
    val tppHQ = createTppAndMPDAgencies().tpp
    val tppChild = createAgency(
      "tppChild",
      AgencyType.Store,
      Full(tppHQ),
      "tppChild",
      false
    )
    val tppChildSibling = createAgency(
      "tppChildSibling",
      AgencyType.Store,
      Full(tppHQ),
      "tppChild",
      false
    )
    val tppGrandchild = createAgency(
      "tppGrandChild",
      AgencyType.Store,
      Full(tppChild),
      "tppGrandchild",
      false
    )
    val tpp = makeUsersForAgency(tppHQ)
    val child = makeUsersForAgency(tppChild)
    val childSibling = makeUsersForAgency(tppChildSibling)
    val childSiblingSecond = makeUsersForAgency(tppChildSibling)
    val grandChild = makeUsersForAgency(tppGrandchild)

    val expectedResult = List(tpp, child, childSibling, childSiblingSecond, grandChild).map(_.id.get)
    val result = Agency.getAllChildrenCustomers(tppHQ.reload).map(_.id.get)

    result should contain theSameElementsAs expectedResult
    cleanUpSuccess()
  }

  private def makeUsersForAgency(agency: Agency) = {
    User.createNewUser(
      firstName = "John",
      lastName = "Doe",
      stripeId = "cus_1234",
      email = "john@example.com",
      password = "1234",
      phone = "123456789",
      coupon = Empty,
      referer = Full(agency),
      agency = Empty,
      UserType.Parent,
      ""
    )
  }
}