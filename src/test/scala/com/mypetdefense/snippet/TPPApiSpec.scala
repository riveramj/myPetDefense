package com.mypetdefense.snippet

import com.mypetdefense.helpers.DBTest
import net.liftweb.common.{Box, Full}
import net.liftweb.http.{JsonResponse, LiftResponse}
import net.liftweb.json._
import JsonDSL._
import com.mypetdefense.helpers.GeneralDbUtils.createTppAndMPDAgencies
import com.mypetdefense.model.Agency
import net.liftweb.json.compactRender
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.mockweb.MockWeb.{testReq, testS}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TPPApiSpec extends DBTest {
  MockWeb.useLiftRulesGlobally = true

  private def runApiRequest(
    url: String,
    requestModifier: (MockHttpServletRequest)=>Any = (req)=>false
  )(responseHandler: (Box[LiftResponse])=>Any) = {
    val mockReq = new MockHttpServletRequest("http://dev.mypetdefense.com" + url)
    requestModifier(mockReq)

    testS(mockReq) {
      testReq(mockReq) {req =>

        responseHandler(TPPApi(req)())
      }
    }
  }

  ignore should "create a new TPP order" in {
    def requestTransformer(request: MockHttpServletRequest) = {
      val tppAgency: Agency = createTppAndMPDAgencies().tpp

      request.method = "POST"
      request.body = {
        compactRender(
          value =
            ("parent" ->
              ("firstName" -> "Mike") ~
              ("lastName" -> "Rivera") ~
              ("email" -> "foo@foo.com") ~
              ("phone" -> "123-123-1234") ~
              ("stripeToken" -> "token") ~
              ("address" ->
                ("street1" -> "123 main st") ~
                ("street2" -> "") ~
                ("city" -> "atlanta") ~
                ("state" -> "GA") ~
                ("zip" -> "30332")
              )
            ) ~
            ("agentId" -> "Mike") ~
            ("storeCode" -> "TPP") ~
            ("pets" -> List(
              ("name" -> "spot small") ~
              ("whelpDate" -> "2018-9-17") ~
              ("product" -> "ZoGuard Plus for dogs") ~
              ("currentSize" -> "small") ~
              ("breed" -> "Akita")
            ))
        ).getBytes("UTF-8")
      }
    }


    runApiRequest("/api/v1/customer", requestTransformer) {
      case Full(JsonResponse(json, _, _, code)) =>
        code should equal (200)

      case somethingUnexpected => fail(somethingUnexpected.toString)
    }
  }
}