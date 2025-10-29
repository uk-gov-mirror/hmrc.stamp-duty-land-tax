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

package uk.gov.hmrc.stampdutylandtax.controllers

import base.SpecBase
import models.{AgentDetailsAfterCreation, AgentDetailsBeforeCreation}
import org.mockito.ArgumentMatchers.any
import play.api.http.Status.{BAD_GATEWAY, BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, OK}
import org.mockito.ArgumentMatchers.eq as eqTo
import play.api.test.Helpers.{contentAsJson, status}
import org.mockito.Mockito.{verify, when}
import play.api.libs.json.Json
import play.api.mvc.Result
import service.ManageAgentsService
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}

import scala.concurrent.{ExecutionContext, Future}

class ManageAgentsControllerSpec extends SpecBase {

  "ManageAgentsController" - {

    "GET agent-details/storn/:storn (getAgentDetails)" - {

      "return OK with agent details when service returns agent details" in new BaseSetup {
        when(mockManageAgentsService.getAgentDetails(eqTo("A-123"), eqTo("B-345"))(any[HeaderCarrier]))
          .thenReturn(Future.successful(Some(testAgentDetailsAfterCreation)))

        val result: Future[Result] = controller.getAgentDetails("A-123", "B-345")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testAgentDetailsAfterCreation)
        verify(mockManageAgentsService).getAgentDetails(eqTo("A-123"), eqTo("B-345"))(any[HeaderCarrier])
      }

      "return 404 with message when service returns None" in new BaseSetup {
        when(mockManageAgentsService.getAgentDetails(eqTo("A-123"), eqTo("B-345"))(any[HeaderCarrier]))
          .thenReturn(Future.successful(None))

        val result: Future[Result] = controller.getAgentDetails("A-123", "B-345")(fakeRequest)

        status(result) mustBe NOT_FOUND
        (contentAsJson(result) \ "message").as[String] mustBe "Agent details not found"
        verify(mockManageAgentsService).getAgentDetails(eqTo("A-123"), eqTo("B-345"))(any[HeaderCarrier])
      }

      "propagate UpstreamErrorResponse status & message" in new BaseSetup {
        when(mockManageAgentsService.getAgentDetails(eqTo("A-123"), eqTo("B-345"))(any[HeaderCarrier]))
          .thenReturn(Future.failed(UpstreamErrorResponse("boom from upstream", BAD_GATEWAY)))

        val result: Future[Result] = controller.getAgentDetails("A-123", "B-345")(fakeRequest)

        status(result) mustBe BAD_GATEWAY
        (contentAsJson(result) \ "message").as[String] must include("boom from upstream")
      }

      "return 500 Unexpected error on unknown exception" in new BaseSetup {
        when(mockManageAgentsService.getAgentDetails(eqTo("A-123"), eqTo("B-345"))(any[HeaderCarrier]))
          .thenReturn(Future.failed(new RuntimeException("unexpected")))

        val result: Future[Result] = controller.getAgentDetails("A-123", "B-345")(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        (contentAsJson(result) \ "message").as[String] must equal("Unexpected error")
      }
    }
    "GET agent-details/get-all-agents/storn/:storn (getAllAgents)" - {

      "return OK with agent details when service returns agent details" in new BaseSetup {
        when(mockManageAgentsService.getAllAgents(eqTo("A-123"))(any[HeaderCarrier]))
          .thenReturn(Future.successful(testAgentDetailsList))

        val result: Future[Result] = controller.getAllAgents("A-123")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testAgentDetailsList)
        verify(mockManageAgentsService).getAllAgents(eqTo("A-123"))(any[HeaderCarrier])
      }

      "return OK with message when service returns an empty list" in new BaseSetup {
        when(mockManageAgentsService.getAllAgents(eqTo("A-123"))(any[HeaderCarrier]))
          .thenReturn(Future.successful(Nil))

        val result: Future[Result] = controller.getAllAgents("A-123")(fakeRequest)

        status(result) mustBe OK
        verify(mockManageAgentsService).getAllAgents(eqTo("A-123"))(any[HeaderCarrier])
      }

      "propagate UpstreamErrorResponse status & message" in new BaseSetup {
        when(mockManageAgentsService.getAllAgents(eqTo("A-123"))(any[HeaderCarrier]))
          .thenReturn(Future.failed(UpstreamErrorResponse("boom from upstream", BAD_GATEWAY)))

        val result: Future[Result] = controller.getAllAgents("A-123")(fakeRequest)

        status(result) mustBe BAD_GATEWAY
        (contentAsJson(result) \ "message").as[String] must include("boom from upstream")
      }

      "return 500 Unexpected error on unknown exception" in new BaseSetup {
        when(mockManageAgentsService.getAllAgents(eqTo("A-123"))(any[HeaderCarrier]))
          .thenReturn(Future.failed(new RuntimeException("unexpected")))

        val result: Future[Result] = controller.getAllAgents("A-123")(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        (contentAsJson(result) \ "message").as[String] must equal("Unexpected error")
      }
    }
    "POST /agent-details/submit (submitAgentDetails)" - {

      "return OK with agent details when service returns agent details" in new BaseSetup {
        when(mockManageAgentsService.submitAgentDetails(any[AgentDetailsBeforeCreation])(any[HeaderCarrier]))
          .thenReturn(Future.successful(testAgentDetailsSuccessResponse))

        val result: Future[Result] = controller.submitAgentDetails(fakeRequest.withBody(Json.toJson(testAgentDetailsBeforeCreation)))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testAgentDetailsSuccessResponse)
        verify(mockManageAgentsService).submitAgentDetails(eqTo(testAgentDetailsBeforeCreation))(any[HeaderCarrier])
      }

      "return BAD_REQUEST with message when given an invalid json body" in new BaseSetup {
        when(mockManageAgentsService.submitAgentDetails(any[AgentDetailsBeforeCreation])(any[HeaderCarrier]))
          .thenReturn(Future.successful(testAgentDetailsSuccessResponse))

        val result: Future[Result] = controller.submitAgentDetails(fakeRequest.withBody(Json.toJson(Json.obj())))

        status(result) mustBe BAD_REQUEST
      }

      "propagate UpstreamErrorResponse status & message" in new BaseSetup {
        when(mockManageAgentsService.submitAgentDetails(any[AgentDetailsBeforeCreation])(any[HeaderCarrier]))
          .thenReturn(Future.failed(UpstreamErrorResponse("boom from upstream", BAD_GATEWAY)))

        val result: Future[Result] = controller.submitAgentDetails(fakeRequest.withBody(Json.toJson(testAgentDetailsBeforeCreation)))

        status(result) mustBe BAD_GATEWAY
        (contentAsJson(result) \ "message").as[String] must include("boom from upstream")
      }

      "return 500 Unexpected error on unknown exception" in new BaseSetup {
        when(mockManageAgentsService.submitAgentDetails(any[AgentDetailsBeforeCreation])(any[HeaderCarrier]))
          .thenReturn(Future.failed(new RuntimeException("unexpected")))

        val result: Future[Result] = controller.submitAgentDetails(fakeRequest.withBody(Json.toJson(testAgentDetailsBeforeCreation)))

        status(result) mustBe INTERNAL_SERVER_ERROR
        (contentAsJson(result) \ "message").as[String] must equal("Unexpected error")
      }
    }

    "GET agent-details/remove (removeAgentDetails)" - {

      "return OK with true when service returns true" in new BaseSetup {
        when(mockManageAgentsService.removeAgent(eqTo("A-123"), eqTo("B-123"))(any[HeaderCarrier]))
          .thenReturn(Future.successful(true))

        val result: Future[Result] = controller.removeAgent("A-123", "B-123")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(true)
        verify(mockManageAgentsService).removeAgent(eqTo("A-123"), eqTo("B-123"))(any[HeaderCarrier])
      }

      "return OK with false when service returns false" in new BaseSetup {
        when(mockManageAgentsService.removeAgent(eqTo("A-123"), eqTo("B-123"))(any[HeaderCarrier]))
          .thenReturn(Future.successful(false))

        val result: Future[Result] = controller.removeAgent("A-123", "B-123")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(false)
        verify(mockManageAgentsService).removeAgent(eqTo("A-123"), eqTo("B-123"))(any[HeaderCarrier])
      }

      "propagate UpstreamErrorResponse status & message" in new BaseSetup {
        when(mockManageAgentsService.removeAgent(eqTo("A-123"), eqTo("B-123"))(any[HeaderCarrier]))
          .thenReturn(Future.failed(UpstreamErrorResponse("boom from upstream", BAD_GATEWAY)))

        val result: Future[Result] = controller.removeAgent("A-123", "B-123")(fakeRequest)

        status(result) mustBe BAD_GATEWAY
        (contentAsJson(result) \ "message").as[String] must include("boom from upstream")
      }

      "return 500 Unexpected error on unknown exception" in new BaseSetup {
        when(mockManageAgentsService.removeAgent(eqTo("A-123"), eqTo("B-123"))(any[HeaderCarrier]))
          .thenReturn(Future.failed(new RuntimeException("unexpected")))

        val result: Future[Result] = controller.removeAgent("A-123", "B-123")(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        (contentAsJson(result) \ "message").as[String] must equal("Unexpected error")
      }
    }
  }

  private trait BaseSetup {
    val mockManageAgentsService: ManageAgentsService = mock[ManageAgentsService]
    implicit val ec: ExecutionContext = cc.executionContext
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val controller = new ManageAgentsController(cc, mockManageAgentsService)
  }
}
