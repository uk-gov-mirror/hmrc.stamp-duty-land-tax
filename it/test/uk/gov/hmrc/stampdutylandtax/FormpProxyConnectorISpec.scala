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

import com.github.tomakehurst.wiremock.client.WireMock.{
  aResponse,
  equalToJson,
  post,
  stubFor,
  urlPathEqualTo
}
import itutil.ApplicationWithWiremock
import models.{AgentDetailsRequest, AgentDetailsResponse}
import models.manage.ReturnsResponse
import models.response.SubmitAgentDetailsResponse
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.Status._
import play.api.libs.json.{JsBoolean, Json}
import uk.gov.hmrc.http.HeaderCarrier

class FormpProxyConnectorISpec extends AnyWordSpec
  with Matchers
  with ScalaFutures
  with IntegrationPatience
  with ApplicationWithWiremock {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val connector: FormpProxyConnector = app.injector.instanceOf[FormpProxyConnector]

  private val storn = "STN001"
  private val arn   = "ARN001"

  "getAgentDetails" should {

    val url = "/stamp-duty-land-tax-stub/manage-agents/agent-details"

    "return AgentDetails when BE returns 200 with valid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(
                """{
                  |  "agentName": "Sunrise Realty",
                  |  "houseNumber": "8B",
                  |  "addressLine1": "Baker Street",
                  |  "addressLine2": null,
                  |  "addressLine3": "Manchester",
                  |  "addressLine4": null,
                  |  "postcode": "M1 2AB",
                  |  "phone": "01611234567",
                  |  "email": "contact@sunriserealty.co.uk",
                  |  "agentReferenceNumber": "ARN001"
                  |}""".stripMargin
              )
          )
      )

      val result = connector.getAgentDetails(storn, arn).futureValue

      result mustBe Some(AgentDetailsResponse(
        agentName            = "Sunrise Realty",
        houseNumber          = "8B",
        addressLine1         = "Baker Street",
        addressLine2         = None,
        addressLine3         = "Manchester",
        addressLine4         = None,
        postcode             = Some("M1 2AB"),
        phone                = Some("01611234567"),
        email                = "contact@sunriserealty.co.uk",
        agentReferenceNumber = "ARN001"
      ))
    }

    "fail when BE returns 200 with invalid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connector.getAgentDetails(storn, arn).futureValue
      }
      ex.getMessage.toLowerCase must include ("error")
    }

    "propagate an upstream error when BE returns 500" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.getAgentDetails(storn, arn).futureValue
      }
      ex.getMessage must include ("returned 500")
    }
  }

  "submitAgentDetails" should {

    val url = "/stamp-duty-land-tax-stub/manage-agents/agent-details/submit"

    val payload = AgentDetailsRequest(
      agentName   = "Acme Property Agents Ltd",
      houseNumber = "42",
      addressLine1 = "High Street",
      addressLine2 = Some("Westminster"),
      addressLine3 = "London",
      addressLine4 = Some("Greater London"),
      postcode     = Some("SW1A 2AA"),
      phone        = Some("02079460000"),
      email        = "info@acmeagents.co.uk"
    )

    "return SubmitAgentDetailsResponse when BE returns 200 with valid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "agentResourceRef": "ARN4324234" }"""))
      )

      val result = connector.submitAgentDetails(payload).futureValue
      result mustBe SubmitAgentDetailsResponse(agentResourceRef = "ARN4324234")
    }

    "fail when BE returns 200 with invalid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connector.submitAgentDetails(payload).futureValue
      }
      ex.getMessage.toLowerCase must include ("agentresourceref")
    }

    "propagate an upstream error when BE returns 500" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(Json.stringify(Json.toJson(payload)), true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.submitAgentDetails(payload).futureValue
      }
      ex.getMessage must include ("returned 500")
    }
  }

  "getAllAgents" should {

    val url = "/stamp-duty-land-tax-stub/manage-agents/agent-details/get-all-agents"

    "return a list of AgentDetails when BE returns 200 with valid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(
                """[
                  |  {
                  |    "agentName": "Acme Property Agents Ltd",
                  |    "houseNumber": "42",
                  |    "addressLine1": "High Street",
                  |    "addressLine2": "Westminster",
                  |    "addressLine3": "London",
                  |    "addressLine4": "Greater London",
                  |    "postcode": "SW1A 2AA",
                  |    "phone": "02079460000",
                  |    "email": "info@acmeagents.co.uk",
                  |    "agentReferenceNumber": "ARN001"
                  |  },
                  |  {
                  |    "agentName": "Harborview Estates",
                  |    "houseNumber": "22A",
                  |    "addressLine1": "Queensway",
                  |    "addressLine2": null,
                  |    "addressLine3": "Birmingham",
                  |    "addressLine4": null,
                  |    "postcode": "B2 4ND",
                  |    "phone": "01214567890",
                  |    "email": "info@harborviewestates.co.uk",
                  |    "agentReferenceNumber": "ARN002"
                  |  }
                  |]""".stripMargin)
          )
      )

      val result = connector.getAllAgents(storn).futureValue

      result mustBe List(
        AgentDetailsResponse(
          agentName            = "Acme Property Agents Ltd",
          houseNumber          = "42",
          addressLine1         = "High Street",
          addressLine2         = Some("Westminster"),
          addressLine3         = "London",
          addressLine4         = Some("Greater London"),
          postcode             = Some("SW1A 2AA"),
          phone                = Some("02079460000"),
          email                = "info@acmeagents.co.uk",
          agentReferenceNumber = "ARN001"
        ),
        AgentDetailsResponse(
          agentName            = "Harborview Estates",
          houseNumber          = "22A",
          addressLine1         = "Queensway",
          addressLine2         = None,
          addressLine3         = "Birmingham",
          addressLine4         = None,
          postcode             = Some("B2 4ND"),
          phone                = Some("01214567890"),
          email                = "info@harborviewestates.co.uk",
          agentReferenceNumber = "ARN002"
        )
      )
    }

    "fail when BE returns 200 with invalid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connector.getAllAgents(storn).futureValue
      }
      ex.getMessage.toLowerCase must include ("error")
    }

    "propagate an upstream error when BE returns 500" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.getAllAgents(storn).futureValue
      }
      ex.getMessage must include ("returned 500")
    }
  }

  "removeAgent" should {

    val url = "/stamp-duty-land-tax-stub/manage-agents/agent-details/remove"

    "return true when BE returns 200 with valid JSON boolean" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody(Json.stringify(JsBoolean(true))))
      )

      val result = connector.removeAgent(storn, arn).futureValue
      result mustBe true
    }

    "fail when BE returns 200 with invalid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connector.removeAgent(storn, arn).futureValue
      }
      ex.getMessage.toLowerCase must include ("jsboolean")
    }

    "propagate an upstream error when BE returns 500" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn","agentReferenceNumber":"$arn"}""", true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.removeAgent(storn, arn).futureValue
      }
      ex.getMessage must include ("returned 500")
    }
  }

  "getReturns" should {

    val url = "/stamp-duty-land-tax-stub/manage-returns/get-all"

    "return ReturnsResponse when BE returns 200 with valid JSON" in {
      val body =
        s"""
           |{
           |  "storn": "$storn",
           |  "returnSummaryCount": 3
           |}
         """.stripMargin

      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody(body))
      )

      val result = connector.getReturns(storn).futureValue
      result mustBe Some(ReturnsResponse(storn = storn, returnSummaryCount = 3))
    }

    "fail when BE returns 200 with invalid JSON" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(OK).withBody("""{ "unexpectedField": true }"""))
      )

      val ex = intercept[Exception] {
        connector.getReturns(storn).futureValue
      }
      ex.getMessage.toLowerCase must include ("error")
    }

    "propagate an upstream error when BE returns 500" in {
      stubFor(
        post(urlPathEqualTo(url))
          .withRequestBody(equalToJson(s"""{"storn":"$storn"}""", true, true))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.getReturns(storn).futureValue
      }
      ex.getMessage must include ("returned 500")
    }
  }
}
