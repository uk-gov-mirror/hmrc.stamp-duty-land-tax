/*
 * Copyright 2025 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package connectors

import com.github.tomakehurst.wiremock.client.WireMock.*
import itutil.ApplicationWithWiremock
import models.agent.*
import models.manage.{ReturnSummary, SdltReturnRecordRequest, SdltReturnRecordResponse}
import models.polling.SubmissionsForPollingResponse
import models.purge.{DeleteReturnRequest, DeleteReturnResponse, GetReturnsForPurgeRequest, ReturnsForPurgeResponse}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.HeaderNames.AUTHORIZATION
import play.api.http.Status.*
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate

class FormpProxyConnectorISpec extends AnyWordSpec
  with Matchers
  with ScalaFutures
  with IntegrationPatience
  with ApplicationWithWiremock {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val connectorWithStub: FormpProxyConnector = appWithSubOn.injector.instanceOf[FormpProxyConnector]
  private val connectorWithFormP: FormpProxyConnector = appWithSubOff.injector.instanceOf[FormpProxyConnector]

  private val internalAuthToken: String = appWithSubOff.configuration.get[String]("internal-auth.token")

  private val storn = "STN001"
  private val arn   = "ARN001"

  // Match either downstream base path so the stub matches whichever URL the connector resolves to.
  private def eitherBase(suffix: String): String = s"/(?:stamp-duty-land-tax-stub|formp-proxy)$suffix"

  "submitAgentDetails" should {

    val anyUrl = eitherBase("/create/predefined-agent")

    val payload = CreatePredefinedAgentRequest(
      storn       = "STN001",
      agentName   = "Acme Property Agents Ltd",
      addressLine1 = Some("42 High Street"),
      addressLine2 = Some("Westminster"),
      addressLine3 = Some("London"),
      addressLine4 = Some("Greater London"),
      postcode     = Some("SW1A 2AA"),
      phone        = Some("02079460000"),
      email        = Some("info@acmeagents.co.uk")
    )

    "select stubUrl when stubFormPBool = true and return CreatePredefinedAgentResponse when BE returns OK with valid JSON" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "agentResourceRef": "ARN4324234", "agentId" : "1234" }"""))
      )

      val result = connectorWithStub.submitAgentDetails(payload).futureValue
      result mustBe CreatePredefinedAgentResponse(agentResourceRef = "ARN4324234", agentId = "1234")
    }
    "select formpUrl when stubFormPBool = false and return CreatePredefinedAgentResponse when BE returns OK with valid JSON" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "agentResourceRef": "ARN4324234", "agentId" : "1234" }"""))
      )

      val result = connectorWithFormP.submitAgentDetails(payload).futureValue
      result mustBe CreatePredefinedAgentResponse(agentResourceRef = "ARN4324234", agentId = "1234")
    }

    "fail when BE returns OK with invalid JSON" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connectorWithStub.submitAgentDetails(payload).futureValue
      }
      ex.getMessage.toLowerCase must include ("agentresourceref")
    }

    "propagate an upstream error when BE returns INTERNAL_SERVER_ERROR" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connectorWithStub.submitAgentDetails(payload).futureValue
      }
      ex.getMessage must include ("returned 500")
    }
  }

  "deletePredefinedAgent" should {

    val anyUrl = eitherBase("/delete/predefined-agent")

    val req = DeletePredefinedAgentRequest(storn, arn)

    "select formPUrl when stubFormPBool = true and return OK with valid JSON object" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "deleted": true }"""))
      )

      val result = connectorWithFormP.deletePredefinedAgent(req).futureValue
      result mustBe DeletePredefinedAgentResponse(true)
    }

    "select stubURL when stubFormPBool = false and return OK with valid JSON object" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "deleted": true }"""))
      )

      val result = connectorWithStub.deletePredefinedAgent(req).futureValue
      result mustBe DeletePredefinedAgentResponse(true)
    }

    "propagate an upstream error when BE returns INTERNAL_SERVER_ERROR" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connectorWithStub.deletePredefinedAgent(req).futureValue
      }
      ex.getMessage must include ("boom")
    }
    "propagate Exception  when BE throws unexpected JSResult " in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{deleted": "ANY_VALUE}"""))
      )

      val ex = intercept[Exception] {
        connectorWithStub.deletePredefinedAgent(req).futureValue
      }
      ex mustBe a[RuntimeException]
    }
  }

  "getReturns" should {

    val anyUrl = eitherBase("/returns")

    val request = SdltReturnRecordRequest(storn = storn, None, false, None)

    "select formPUrl when stubFormPBool = false, return SdltReturnRecordResponse when BE returns OK with valid JSON" in {

      val responseBody =
        s"""
           |{
           |  "storn": "$storn",
           |  "returnSummaryCount": 1,
           |  "returnSummaryList": [
           |    {
           |      "returnReference": "RET123",
           |      "utrn": "UTRN001",
           |      "status": "SUBMITTED",
           |      "dateSubmitted": "2025-11-10",
           |      "purchaserName": "John Smith",
           |      "address": "10 Downing Street, London",
           |      "agentReference": "Smith & Co"
           |    }
           |  ]
           |}
         """.stripMargin

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(request)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody(responseBody))
      )

      val result = connectorWithFormP.getReturns(request).futureValue
      result mustBe SdltReturnRecordResponse(
        returnSummaryCount = Some(1),
        returnSummaryList = List(
          ReturnSummary(
            returnReference = "RET123",
            utrn = Some("UTRN001"),
            status = Some("SUBMITTED"),
            dateSubmitted = Some(LocalDate.parse("2025-11-10")),
            purchaserName = "John Smith",
            address = "10 Downing Street, London",
            agentReference = Some("Smith & Co")
          )
        )
      )
    }
    "select stubUrl when stubFormPBool = true, return SdltReturnRecordResponse when BE returns OK with valid JSON" in {

      val responseBody =
        s"""
           |{
           |  "storn": "$storn",
           |  "returnSummaryCount": 1,
           |  "returnSummaryList": [
           |    {
           |      "returnReference": "RET123",
           |      "utrn": "UTRN001",
           |      "status": "SUBMITTED",
           |      "dateSubmitted": "2025-11-10",
           |      "purchaserName": "John Smith",
           |      "address": "10 Downing Street, London",
           |      "agentReference": "Smith & Co"
           |    }
           |  ]
           |}
         """.stripMargin

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(request)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody(responseBody))
      )

      val result = connectorWithStub.getReturns(request).futureValue
      result mustBe SdltReturnRecordResponse(
        returnSummaryCount = Some(1),
        returnSummaryList = List(
          ReturnSummary(
            returnReference = "RET123",
            utrn = Some("UTRN001"),
            status = Some("SUBMITTED"),
            dateSubmitted = Some(LocalDate.parse("2025-11-10")),
            purchaserName = "John Smith",
            address = "10 Downing Street, London",
            agentReference = Some("Smith & Co")
          )
        )
      )
    }
    "return SdltReturnRecordResponse when BE returns OK with valid JSON" in {

      val responseBody =
        s"""
           |{
           |  "storn": "$storn",
           |  "returnSummaryCount": 1,
           |  "returnSummaryList": [
           |    {
           |      "returnReference": "RET123",
           |      "utrn": "UTRN001",
           |      "status": "SUBMITTED",
           |      "dateSubmitted": "2025-11-10",
           |      "purchaserName": "John Smith",
           |      "address": "10 Downing Street, London",
           |      "agentReference": "Smith & Co"
           |    }
           |  ]
           |}
         """.stripMargin

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(request)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody(responseBody))
      )

      val result = connectorWithStub.getReturns(request).futureValue
      result mustBe SdltReturnRecordResponse(
        returnSummaryCount = Some(1),
        returnSummaryList = List(
          ReturnSummary(
            returnReference = "RET123",
            utrn = Some("UTRN001"),
            status = Some("SUBMITTED"),
            dateSubmitted = Some(LocalDate.parse("2025-11-10")),
            purchaserName = "John Smith",
            address = "10 Downing Street, London",
            agentReference = Some("Smith & Co")
          )
        )
      )
    }

    "fail when BE returns OK with invalid JSON" in {

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(request)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connectorWithStub.getReturns(request).futureValue
      }
      ex.getMessage.toLowerCase must include("error")
    }

    "propagate an upstream error when BE returns INTERNAL_SERVER_ERROR" in {

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(request)), true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connectorWithStub.getReturns(request).futureValue
      }
      ex.getMessage must include("returned 500")
    }
  }

  "getSdltOrganisation" should {

    val anyUrl = eitherBase("/organisation")

    "select formUrl when stubFormPBool = false and return SdltOrganisation when BE returns OK with valid JSON" in {
      val responseJson =
        s"""
           |{
           |  "storn": "STN001",
           |  "version": "1",
           |  "isReturnUser": "true",
           |  "doNotDisplayWelcomePage": "Yes",
           |  "agents": [
           |    {
           |      "agentResourceReference": "ARN001",
           |      "name": "John",
           |      "storn": "STN001",
           |      "agentId": "AGT001",
           |      "address1": "1 High Street",
           |      "address2": "Westminster",
           |      "address3": "London",
           |      "address4": "Greater London",
           |      "postcode": "SW72AZ",
           |      "phone": "02079460000",
           |      "email": "info@acme.co.uk"
           |    }
           |  ]
           |}
           |
         """.stripMargin

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody(responseJson))
      )

      val result = connectorWithFormP.getSdltOrganisation(storn).futureValue

      result mustBe SdltOrganisationResponse(
        storn = storn,
        version = Some("1"),
        isReturnUser = Some("true"),
        doNotDisplayWelcomePage = Some("Yes"),
        agents = Seq(
          CreatedAgent(
            storn                  = Some(storn),
            agentId                = Some("AGT001"),
            name                   = Some("John"),
            houseNumber            = None,
            address1               = Some("1 High Street"),
            address2               = Some("Westminster"),
            address3               = Some("London"),
            address4               = Some("Greater London"),
            postcode               = Some("SW72AZ"),
            phone                  = Some("02079460000"),
            email                  = Some("info@acme.co.uk"),
            dxAddress              = None,
            agentResourceReference = Some("ARN001")
          )
        )
      )

      result.toString must include("SW72AZ")
      result.toString must include("AGT001")
      result.toString must include("John")
      result.toString must include("ARN001")
      result.toString must include("02079460000")
      result.toString must include("1 High Street")
      result.toString must include("Westminster")
      result.toString must include("London")
      result.toString must include("Greater London")
      result.toString must include("info@acme.co.uk")
    }
    "select stubUrl when stubFormPBool = true and return SdltOrganisation when BE returns OK with valid JSON" in {
      val responseJson =
        s"""
           |{
           |  "storn": "STN001",
           |  "version": "1",
           |  "isReturnUser": "true",
           |  "doNotDisplayWelcomePage": "Yes",
           |  "agents": [
           |    {
           |      "agentResourceReference": "ARN001",
           |      "name": "John",
           |      "storn": "STN001",
           |      "agentId": "AGT001",
           |      "address1": "1 High Street",
           |      "address2": "Westminster",
           |      "address3": "London",
           |      "address4": "Greater London",
           |      "postcode": "SW72AZ",
           |      "phone": "02079460000",
           |      "email": "info@acme.co.uk"
           |    }
           |  ]
           |}
           |
         """.stripMargin

      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody(responseJson))
      )

      val result = connectorWithStub.getSdltOrganisation(storn).futureValue

      result mustBe SdltOrganisationResponse(
        storn = storn,
        version = Some("1"),
        isReturnUser = Some("true"),
        doNotDisplayWelcomePage = Some("Yes"),
        agents = Seq(
          CreatedAgent(
            storn                  = Some(storn),
            agentId                = Some("AGT001"),
            name                   = Some("John"),
            houseNumber            = None,
            address1               = Some("1 High Street"),
            address2               = Some("Westminster"),
            address3               = Some("London"),
            address4               = Some("Greater London"),
            postcode               = Some("SW72AZ"),
            phone                  = Some("02079460000"),
            email                  = Some("info@acme.co.uk"),
            dxAddress              = None,
            agentResourceReference = Some("ARN001")
          )
        )
      )

      result.toString must include("SW72AZ")
      result.toString must include("AGT001")
      result.toString must include("John")
      result.toString must include("ARN001")
      result.toString must include("02079460000")
      result.toString must include("1 High Street")
      result.toString must include("Westminster")
      result.toString must include("London")
      result.toString must include("Greater London")
      result.toString must include("info@acme.co.uk")
    }

    "fail when BE returns OK with invalid JSON" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connectorWithStub.getSdltOrganisation(storn).futureValue
      }
      ex.getMessage.toLowerCase must include("error")
    }

    "propagate an upstream error when BE returns INTERNAL_SERVER_ERROR" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connectorWithStub.getSdltOrganisation(storn).futureValue
      }
      ex.getMessage must include("returned 500")
    }
  }

  "updateAgentDetails" should {
    val anyUrl = eitherBase("/update/predefined-agent")

    val agentName = "John Snow"
    val req = UpdatePredefinedAgentRequest(
      arn,
      storn,
      agentName,
      Some("value"),
      None,
      None,
      Some("value"),
      None,
      Some("value"),
      Some("value"),
      Some("value"),
      None
    )

    val payLoad = Json.toJson(req)

    "select formPUrl when stubFormPBool = true and return CREATED with valid JSON object" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(payLoad), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "updated": true }"""))
      )

      val result = connectorWithFormP.updateAgentDetails(req).futureValue
      result mustBe UpdatePredefinedAgentResponse(true)
    }

    "select stubURL when stubFormPBool = false and return OK with valid JSON object" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(payLoad), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "updated": true }"""))
      )

      val result = connectorWithStub.updateAgentDetails(req).futureValue
      result mustBe UpdatePredefinedAgentResponse(true)
    }

    "propagate an upstream error when BE returns INTERNAL_SERVER_ERROR" in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(payLoad), true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connectorWithStub.updateAgentDetails(req).futureValue
      }
      ex.getMessage must include("boom")
    }
    "propagate Exception  when BE throws unexpected JSResult " in {
      stubFor(
        post(urlPathMatching(anyUrl))
          .withRequestBody(equalToJson(Json.stringify(payLoad), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{deleted": "ANY_VALUE}"""))
      )

      val ex = intercept[Exception] {
        connectorWithStub.updateAgentDetails(req).futureValue
      }
      ex mustBe a[RuntimeException]
    }
  }

  "getSubmissionsForPolling" should {

    val formpUrl = "/formp-proxy/submissions-polling"

    "present the internal-auth token and return the submissions due for polling" in {
      stubFor(
        get(urlPathEqualTo(formpUrl))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "submissions": [] }"""))
      )

      connectorWithFormP.getSubmissionsForPolling().futureValue mustBe SubmissionsForPollingResponse(Nil)

      verify(getRequestedFor(urlPathEqualTo(formpUrl)).withHeader(AUTHORIZATION, equalTo(internalAuthToken)))
    }

    "fail with a 401 error when formp-proxy will not accept the token" in {
      stubFor(
        get(urlPathEqualTo(formpUrl))
          .willReturn(aResponse().withStatus(UNAUTHORIZED))
      )

      val ex = intercept[Exception] {
        connectorWithFormP.getSubmissionsForPolling().futureValue
      }
      ex.getMessage must include ("401")
    }
  }

  "getReturnsForPurge" should {

    val formpUrl = "/formp-proxy/returns-for-purge"
    val request  = GetReturnsForPurgeRequest(LocalDate.parse("2026-07-06"))

    "present the internal-auth token and return the returns due for purge" in {
      stubFor(
        post(urlPathEqualTo(formpUrl))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "returnsForPurge": [] }"""))
      )

      connectorWithFormP.getReturnsForPurge(request).futureValue mustBe ReturnsForPurgeResponse(Nil)

      verify(postRequestedFor(urlPathEqualTo(formpUrl)).withHeader(AUTHORIZATION, equalTo(internalAuthToken)))
    }

    "fail with a 401 error when formp-proxy will not accept the token" in {
      stubFor(
        post(urlPathEqualTo(formpUrl))
          .willReturn(aResponse().withStatus(UNAUTHORIZED))
      )

      val ex = intercept[Exception] {
        connectorWithFormP.getReturnsForPurge(request).futureValue
      }
      ex.getMessage must include ("401")
    }
  }

  "deleteReturn" should {

    val formpUrl = "/formp-proxy/delete/return"
    val request  = DeleteReturnRequest(storn = "STN001", returnResourceRef = "9000001")

    "carry the internal-auth token through to formp-proxy and return the delete response" in {
      stubFor(
        post(urlPathEqualTo(formpUrl))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "deleted": true }"""))
      )

      connectorWithFormP.deleteReturn(request).futureValue mustBe DeleteReturnResponse(deleted = true)

      verify(postRequestedFor(urlPathEqualTo(formpUrl)).withHeader(AUTHORIZATION, equalTo(internalAuthToken)))
    }

    "fail with a 401 error when formp-proxy rejects the request" in {
      stubFor(
        post(urlPathEqualTo(formpUrl))
          .willReturn(aResponse().withStatus(UNAUTHORIZED))
      )

      val ex = intercept[Exception] {
        connectorWithFormP.deleteReturn(request).futureValue
      }
      ex.getMessage must include ("401")
    }
  }
}