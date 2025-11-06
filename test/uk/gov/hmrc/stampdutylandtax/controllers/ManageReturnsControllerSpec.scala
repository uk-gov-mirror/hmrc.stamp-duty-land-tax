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
import models.manage.SdltReturnRecordResponse
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.eq as eqTo
import org.mockito.Mockito.{verify, when}
import play.api.http.Status.{BAD_GATEWAY, INTERNAL_SERVER_ERROR, NOT_FOUND, OK}
import play.api.libs.json.Json
import play.api.test.Helpers.{contentAsJson, status}
import play.api.mvc.Result
import service.ManageReturnsService
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}

import scala.concurrent.{ExecutionContext, Future}

class ManageReturnsControllerSpec extends SpecBase {

  "ManageReturnsController" - {

    ".getReturns" - {

      "return OK with returns when service successfully returns a ReturnsResponse payload" in new BaseSetup {
        private val storn = "STN-123"
        private val payload = SdltReturnRecordResponse(
          storn              = storn,
          returnSummaryCount = 3,
          returnSummaryList = Nil
        )

        when(mockManageReturnsService.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.successful(Some(payload)))

        val result: Future[Result] = controller.getReturns(storn)(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(payload)
        verify(mockManageReturnsService).getReturns(eqTo(storn))(any[HeaderCarrier])
      }

      "return NOT_FOUND with message when service fails to retrieve a payload" in new BaseSetup {
        private val storn = "STN-404"

        when(mockManageReturnsService.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.successful(None))

        val result: Future[Result] = controller.getReturns(storn)(fakeRequest)

        status(result) mustBe NOT_FOUND
        (contentAsJson(result) \ "message").as[String] mustBe s"No returns found for storn: $storn"
        verify(mockManageReturnsService).getReturns(eqTo(storn))(any[HeaderCarrier])
      }

      "propagate UpstreamErrorResponse status & message" in new BaseSetup {
        private val storn = "STN-BADGATE"

        when(mockManageReturnsService.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.failed(UpstreamErrorResponse("boom from upstream", BAD_GATEWAY)))

        val result: Future[Result] = controller.getReturns(storn)(fakeRequest)

        status(result) mustBe BAD_GATEWAY
        (contentAsJson(result) \ "message").as[String] must include("boom from upstream")
      }

      "return INTERNAL_SERVER_ERROR Unexpected error on unknown exception" in new BaseSetup {
        private val storn = "STN-ERR"

        when(mockManageReturnsService.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.failed(new RuntimeException("unexpected")))

        val result: Future[Result] = controller.getReturns(storn)(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        (contentAsJson(result) \ "message").as[String] must equal("Unexpected error")
      }
    }
  }

  private trait BaseSetup {
    val mockManageReturnsService: ManageReturnsService = mock[ManageReturnsService]
    implicit val ec: ExecutionContext = cc.executionContext
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val controller = new ManageReturnsController(cc, mockManageReturnsService)
  }
}
