package uk.gov.hmrc.stampdutylandtax.service

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

import base.SpecBase
import connectors.FormpProxyConnector
import models.manage.ReturnsResponse
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{times, verify, when}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import service.ManageReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class ManageReturnsServiceSpec extends Matchers with ScalaFutures with SpecBase {

  "ManageReturnsService" - {

    "getReturns" - {

      "should delegate to FormpProxyConnector and return a successful ReturnsResponse" in new Setup {
        private val storn = "STN-123"
        private val response = ReturnsResponse(storn, 5, Nil)

        when(mockFormpProxyConnector.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.successful(Some(response)))

        val result = service.getReturns(storn).futureValue

        result mustBe Some(response)
        verify(mockFormpProxyConnector, times(1)).getReturns(eqTo(storn))(any[HeaderCarrier])
      }

      "should delegate to FormpProxyConnector and return None when connector fails to find a return" in new Setup {
        private val storn = "STN-NONE"

        when(mockFormpProxyConnector.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.successful(None))

        val result = service.getReturns(storn).futureValue

        result mustBe None
        verify(mockFormpProxyConnector, times(1)).getReturns(eqTo(storn))(any[HeaderCarrier])
      }

      "should propagate exceptions thrown by the connector" in new Setup {
        private val storn = "STN-ERR"

        when(mockFormpProxyConnector.getReturns(eqTo(storn))(any[HeaderCarrier]))
          .thenReturn(Future.failed(new RuntimeException("boom")))

        val ex = intercept[RuntimeException] {
          service.getReturns(storn).futureValue
        }

        ex.getMessage must include("boom")
        verify(mockFormpProxyConnector, times(1)).getReturns(eqTo(storn))(any[HeaderCarrier])
      }
    }
  }

  private trait Setup {
    val mockFormpProxyConnector: FormpProxyConnector = mock[FormpProxyConnector]
    implicit val ec: ExecutionContext = cc.executionContext
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val service = new ManageReturnsService(mockFormpProxyConnector)
  }
}
