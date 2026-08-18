/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.stampdutylandtax.service

import base.SpecBase
import connectors.FilingFormpProxyConnector
import models.submission.*
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.*
import service.filing.ChrisService
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}

import scala.concurrent.Future

class ChrisServiceSpec extends SpecBase {
  
  private def mkLockReturnRequest(
                                   storn: String = "STORN12345",
                                   returnResourceRef: String = "100001",
                                   version: Int = 1
                                 ): LockReturnRequest =
    LockReturnRequest(storn = storn, returnResourceRef = returnResourceRef, version = version)

  private def mkLockReturnResponse(success: Boolean = true): LockReturnResponse =
    LockReturnResponse(success = success)

  private def mkCreateSubmissionRequest(
                                         storn: String = "STORN12345",
                                         returnResourceRef: String = "100001"
                                       ): CreateSubmissionRequest =
    CreateSubmissionRequest(storn = storn, returnResourceRef = returnResourceRef, email = Some("user@example.com"))

  private def mkCreateSubmissionReturn(success: Boolean = true): CreateSubmissionReturn =
    CreateSubmissionReturn(success = success)

  private def mkSubmissionUpdate(): SubmissionUpdate =
    SubmissionUpdate(
      IRMarkRecieved = Some("MARK-IN"),
      utrn = Some("UTRN123"),
      email = Some("user@example.com"),
      submissionRequestDate = Some("2026-01-02T09:00:00Z"),
      acceptedDate = Some("2026-01-02T09:05:00Z"),
      submittableStatus = Some("SUBMITTABLE"),
      govTalkErrorCode = None,
      govTalkErrorType = None,
      govTalkErrorMessage = None,
      IRMarkSent = Some("MARK-OUT")
    )

  private def mkUpdateSubmissionRequest(
                                         storn: String = "STORN12345",
                                         returnResourceRef: String = "100001"
                                       ): UpdateSubmissionRequest =
    UpdateSubmissionRequest(storn = storn, returnResourceRef = returnResourceRef, submission = mkSubmissionUpdate())

  private def mkUpdateSubmissionReturn(success: Boolean = true): UpdateSubmissionReturn =
    UpdateSubmissionReturn(success = success)

  private def mkCreateSubmissionErrorDetailRequest(
                                                    storn: String = "STORN12345",
                                                    returnResourceRef: String = "100001"
                                                  ): CreateSubmissionErrorDetailRequest =
    CreateSubmissionErrorDetailRequest(
      storn = storn,
      returnResourceRef = returnResourceRef,
      submissionErrorDetails = SubmissionErrorDetail(position = "1", errorMessage = "Invalid value")
    )

  private def mkCreateSubmissionErrorDetailReturn(success: Boolean = true): CreateSubmissionErrorDetailReturn =
    CreateSubmissionErrorDetailReturn(success = success)

  private def mkDeleteSubmissionErrorDetailRequest(
                                                    storn: String = "STORN12345",
                                                    returnResourceRef: String = "100001"
                                                  ): DeleteSubmissionErrorDetailRequest =
    DeleteSubmissionErrorDetailRequest(storn = storn, returnResourceRef = returnResourceRef)

  private def mkDeleteSubmissionErrorDetailReturn(success: Boolean = true): DeleteSubmissionErrorDetailReturn =
    DeleteSubmissionErrorDetailReturn(success = success)

  private def mkGovTalkStatusReturn(success: Boolean = true): GovTalkStatusReturn =
    GovTalkStatusReturn(success = success)

  private def mkGovTalkStatusInitial(): GovTalkStatusInitial =
    GovTalkStatusInitial(
      formLock = "N",
      createTimestamp = "2026-01-02T09:00:00Z",
      endStateTimestamp = Some("2026-01-02T09:05:00Z"),
      lastMessageTimestamp = "2026-01-02T09:04:00Z",
      numberOfPolls = "3",
      pollInterval = "10",
      protocolStatus = "SUBMITTED",
      gatewayUrl = "https://gateway.example/submit"
    )

  private def mkInsertInitialGovTalkStatusRequest(
                                                   userIdentifier: String = "USER-1",
                                                   formResultId: String = "FR-1"
                                                 ): InsertInitialGovTalkStatusRequest =
    InsertInitialGovTalkStatusRequest(
      userIdentifier = userIdentifier,
      formResultId = formResultId,
      correlationId = "CORR-1",
      govTalkStatus = mkGovTalkStatusInitial()
    )

  private def mkGovTalkStatusReset(): GovTalkStatusReset =
    GovTalkStatusReset(
      formLock = "N",
      createTimestamp = "2026-01-02T09:00:00Z",
      endStateTimestamp = Some("2026-01-02T09:05:00Z"),
      lastMessageTimestamp = "2026-01-02T09:04:00Z",
      numberOfPolls = "3",
      pollInterval = "10",
      protocolStatusOld = "SUBMITTED",
      protocolStatusNew = "ACKNOWLEDGED",
      gatewayUrl = "https://gateway.example/submit"
    )

  private def mkResetGovTalkStatusRequest(
                                           userIdentifier: String = "USER-1",
                                           formResultId: String = "FR-1"
                                         ): ResetGovTalkStatusRequest =
    ResetGovTalkStatusRequest(
      userIdentifier = userIdentifier,
      formResultId = formResultId,
      correlationId = "CORR-1",
      govTalkStatus = mkGovTalkStatusReset()
    )

  private def mkUpdateGovTalkStatusRequest(
                                            userIdentifier: String = "USER-1",
                                            formResultId: String = "FR-1"
                                          ): UpdateGovTalkStatusRequest =
    UpdateGovTalkStatusRequest(
      userIdentifier = userIdentifier,
      formResultId = formResultId,
      endStateTimestamp = "2026-01-02T09:05:00Z",
      protocolStatus = "ACKNOWLEDGED"
    )

  private def mkUpdateGovTalkStatusCorrelationIdRequest(
                                                         userIdentifier: String = "USER-1",
                                                         formResultId: String = "FR-1"
                                                       ): UpdateGovTalkStatusCorrelationIdRequest =
    UpdateGovTalkStatusCorrelationIdRequest(
      userIdentifier = userIdentifier,
      formResultId = formResultId,
      correlationId = "CORR-1",
      pollInterval = 0,
      gatewayUrl = "http://localhost:6995/chris"
    )

  private def mkUpdateGovTalkStatusLockRequest(
                                                userIdentifier: String = "USER-1",
                                                formResultId: String = "FR-1"
                                              ): UpdateGovTalkStatusLockRequest =
    UpdateGovTalkStatusLockRequest(
      userIdentifier = userIdentifier,
      formResultId = formResultId,
      govTalkStatus = GovTalkStatusLock(
        formLockOld = "N",
        formLockNew = "Y",
        pollInterval = "10",
        gatewayUrl = "https://gateway.example/submit"
      )
    )

  private def mkUpdateGovTalkStatisticsRequest(
                                                userIdentifier: String = "USER-1",
                                                formResultId: String = "FR-1"
                                              ): UpdateGovTalkStatisticsRequest =
    UpdateGovTalkStatisticsRequest(
      userIdentifier = userIdentifier,
      formResultId = formResultId,
      govTalkStatus = GovTalkStatusStatistics(
        lastMessageTimestamp = "2026-01-02T09:04:00Z",
        numberOfPolls = "3",
        pollInterval = "10",
        gatewayUrl = "https://gateway.example/submit"
      )
    )

  private def mkDeleteGovTalkStatusRequest(resultId: String = "FR-1"): DeleteGovTalkStatusRequest =
    DeleteGovTalkStatusRequest(resultId = resultId)

  private def mkSelectGovTalkStatusRequest(
                                            userIdentifier: String = "USER-1",
                                            formResultId: String = "FR-1"
                                          ): SelectGovTalkStatusRequest =
    SelectGovTalkStatusRequest(userIdentifier = userIdentifier, formResultId = formResultId)

  private def mkSelectGovTalkStatusResponse(): SelectGovTalkStatusResponse =
    SelectGovTalkStatusResponse(
      userIdentifier = Some("USER-1"),
      formResultId = Some("FR-1"),
      correlationId = Some("CORR-1"),
      formLock = Some("N"),
      createTimestamp = Some("2026-01-02T09:00:00Z"),
      endStateTimestamp = Some("2026-01-02T09:05:00Z"),
      lastMessageTimestamp = Some("2026-01-02T09:04:00Z"),
      numberOfPolls = Some("3"),
      pollInterval = Some("10"),
      protocolStatus = Some("SUBMITTED"),
      gatewayUrl = Some("https://gateway.example/submit")
    )

  private def mkSelectGovTalkFormResultIdRequest(userIdentifier: String = "USER-1"): SelectGovTalkFormResultIdRequest =
    SelectGovTalkFormResultIdRequest(userIdentifier = userIdentifier)

  private def mkSelectGovTalkFormResultIdResponse(): SelectGovTalkFormResultIdResponse =
    SelectGovTalkFormResultIdResponse(formResultId = Some("FR-1"))

  "ChrisService lockReturn" - {

    "must delegate to connector and return Right (happy path)" in {
      val connector                  = mock[FilingFormpProxyConnector]
      val service                    = new ChrisService(connector)
      val request: LockReturnRequest = mkLockReturnRequest()
      implicit val hc: HeaderCarrier = HeaderCarrier()

      when(connector.lockReturn(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(Right(mkLockReturnResponse())))

      val result = service.lockReturn(request).futureValue
      result mustBe Right(mkLockReturnResponse())

      verify(connector).lockReturn(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must return Left when connector returns Left" in {
      val connector                  = mock[FilingFormpProxyConnector]
      val service                    = new ChrisService(connector)
      val request: LockReturnRequest = mkLockReturnRequest()
      val error                      = UpstreamErrorResponse("Locked", 423)
      implicit val hc: HeaderCarrier = HeaderCarrier()

      when(connector.lockReturn(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(Left(error)))

      val result = service.lockReturn(request).futureValue
      result mustBe Left(error)

      verify(connector).lockReturn(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                  = mock[FilingFormpProxyConnector]
      val service                    = new ChrisService(connector)
      val request: LockReturnRequest = mkLockReturnRequest()
      val error                       = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier = HeaderCarrier()

      when(connector.lockReturn(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.lockReturn(request).failed.futureValue
      ex mustBe error

      verify(connector).lockReturn(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                  = mock[FilingFormpProxyConnector]
      val service                    = new ChrisService(connector)
      val request: LockReturnRequest = mkLockReturnRequest()
      implicit val hc: HeaderCarrier = HeaderCarrier()

      when(connector.lockReturn(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(Right(mkLockReturnResponse())))

      service.lockReturn(request).futureValue

      verify(connector, times(1)).lockReturn(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService createSubmission" - {

    "must delegate to connector (happy path)" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: CreateSubmissionRequest = mkCreateSubmissionRequest()
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.createSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkCreateSubmissionReturn()))

      val result = service.createSubmission(request).futureValue
      result mustBe mkCreateSubmissionReturn()

      verify(connector).createSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: CreateSubmissionRequest = mkCreateSubmissionRequest()
      val error                             = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.createSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.createSubmission(request).failed.futureValue
      ex mustBe error

      verify(connector).createSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: CreateSubmissionRequest = mkCreateSubmissionRequest()
      val error                             = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.createSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.createSubmission(request).failed.futureValue
      ex mustBe error

      verify(connector).createSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: CreateSubmissionRequest = mkCreateSubmissionRequest()
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.createSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkCreateSubmissionReturn()))

      service.createSubmission(request).futureValue

      verify(connector, times(1)).createSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }

  "ChrisService updateSubmission" - {

    "must delegate to connector (happy path)" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: UpdateSubmissionRequest = mkUpdateSubmissionRequest()
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.updateSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkUpdateSubmissionReturn()))

      val result = service.updateSubmission(request).futureValue
      result mustBe mkUpdateSubmissionReturn()

      verify(connector).updateSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: UpdateSubmissionRequest = mkUpdateSubmissionRequest()
      val error                             = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.updateSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateSubmission(request).failed.futureValue
      ex mustBe error

      verify(connector).updateSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: UpdateSubmissionRequest = mkUpdateSubmissionRequest()
      val error                             = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.updateSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateSubmission(request).failed.futureValue
      ex mustBe error

      verify(connector).updateSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                        = mock[FilingFormpProxyConnector]
      val service                          = new ChrisService(connector)
      val request: UpdateSubmissionRequest = mkUpdateSubmissionRequest()
      implicit val hc: HeaderCarrier       = HeaderCarrier()

      when(connector.updateSubmission(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkUpdateSubmissionReturn()))

      service.updateSubmission(request).futureValue

      verify(connector, times(1)).updateSubmission(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }

  "ChrisService createSubmissionErrorDetail" - {

    "must delegate to connector (happy path)" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: CreateSubmissionErrorDetailRequest = mkCreateSubmissionErrorDetailRequest()
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkCreateSubmissionErrorDetailReturn()))

      val result = service.createSubmissionErrorDetail(request).futureValue
      result mustBe mkCreateSubmissionErrorDetailReturn()

      verify(connector).createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: CreateSubmissionErrorDetailRequest = mkCreateSubmissionErrorDetailRequest()
      val error                                        = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.createSubmissionErrorDetail(request).failed.futureValue
      ex mustBe error

      verify(connector).createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: CreateSubmissionErrorDetailRequest = mkCreateSubmissionErrorDetailRequest()
      val error                                        = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.createSubmissionErrorDetail(request).failed.futureValue
      ex mustBe error

      verify(connector).createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: CreateSubmissionErrorDetailRequest = mkCreateSubmissionErrorDetailRequest()
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkCreateSubmissionErrorDetailReturn()))

      service.createSubmissionErrorDetail(request).futureValue

      verify(connector, times(1)).createSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }

  "ChrisService deleteSubmissionErrorDetail" - {

    "must delegate to connector (happy path)" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: DeleteSubmissionErrorDetailRequest = mkDeleteSubmissionErrorDetailRequest()
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkDeleteSubmissionErrorDetailReturn()))

      val result = service.deleteSubmissionErrorDetail(request).futureValue
      result mustBe mkDeleteSubmissionErrorDetailReturn()

      verify(connector).deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: DeleteSubmissionErrorDetailRequest = mkDeleteSubmissionErrorDetailRequest()
      val error                                        = UpstreamErrorResponse("Not found", 404)
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.deleteSubmissionErrorDetail(request).failed.futureValue
      ex mustBe error

      verify(connector).deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: DeleteSubmissionErrorDetailRequest = mkDeleteSubmissionErrorDetailRequest()
      val error                                        = new RuntimeException("Connection timeout")
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.deleteSubmissionErrorDetail(request).failed.futureValue
      ex mustBe error

      verify(connector).deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                                   = mock[FilingFormpProxyConnector]
      val service                                     = new ChrisService(connector)
      val request: DeleteSubmissionErrorDetailRequest = mkDeleteSubmissionErrorDetailRequest()
      implicit val hc: HeaderCarrier                  = HeaderCarrier()

      when(connector.deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkDeleteSubmissionErrorDetailReturn()))

      service.deleteSubmissionErrorDetail(request).futureValue

      verify(connector, times(1)).deleteSubmissionErrorDetail(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService insertInitialGovTalkStatus" - {

    "must delegate to connector (happy path)" in {
      val connector                                  = mock[FilingFormpProxyConnector]
      val service                                    = new ChrisService(connector)
      val request: InsertInitialGovTalkStatusRequest = mkInsertInitialGovTalkStatusRequest()
      implicit val hc: HeaderCarrier                 = HeaderCarrier()

      when(connector.insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.insertInitialGovTalkStatus(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                                  = mock[FilingFormpProxyConnector]
      val service                                    = new ChrisService(connector)
      val request: InsertInitialGovTalkStatusRequest = mkInsertInitialGovTalkStatusRequest()
      val error                                       = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier                 = HeaderCarrier()

      when(connector.insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.insertInitialGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                                  = mock[FilingFormpProxyConnector]
      val service                                    = new ChrisService(connector)
      val request: InsertInitialGovTalkStatusRequest = mkInsertInitialGovTalkStatusRequest()
      val error                                       = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier                 = HeaderCarrier()

      when(connector.insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.insertInitialGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                                  = mock[FilingFormpProxyConnector]
      val service                                    = new ChrisService(connector)
      val request: InsertInitialGovTalkStatusRequest = mkInsertInitialGovTalkStatusRequest()
      implicit val hc: HeaderCarrier                 = HeaderCarrier()

      when(connector.insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.insertInitialGovTalkStatus(request).futureValue

      verify(connector, times(1)).insertInitialGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService resetGovTalkStatus" - {

    "must delegate to connector (happy path)" in {
      val connector                          = mock[FilingFormpProxyConnector]
      val service                            = new ChrisService(connector)
      val request: ResetGovTalkStatusRequest = mkResetGovTalkStatusRequest()
      implicit val hc: HeaderCarrier         = HeaderCarrier()

      when(connector.resetGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.resetGovTalkStatus(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).resetGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                          = mock[FilingFormpProxyConnector]
      val service                            = new ChrisService(connector)
      val request: ResetGovTalkStatusRequest = mkResetGovTalkStatusRequest()
      val error                               = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier         = HeaderCarrier()

      when(connector.resetGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.resetGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).resetGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                          = mock[FilingFormpProxyConnector]
      val service                            = new ChrisService(connector)
      val request: ResetGovTalkStatusRequest = mkResetGovTalkStatusRequest()
      val error                               = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier         = HeaderCarrier()

      when(connector.resetGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.resetGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).resetGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                          = mock[FilingFormpProxyConnector]
      val service                            = new ChrisService(connector)
      val request: ResetGovTalkStatusRequest = mkResetGovTalkStatusRequest()
      implicit val hc: HeaderCarrier         = HeaderCarrier()

      when(connector.resetGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.resetGovTalkStatus(request).futureValue

      verify(connector, times(1)).resetGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService updateGovTalkStatus" - {

    "must delegate to connector (happy path)" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: UpdateGovTalkStatusRequest = mkUpdateGovTalkStatusRequest()
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.updateGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.updateGovTalkStatus(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).updateGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: UpdateGovTalkStatusRequest = mkUpdateGovTalkStatusRequest()
      val error                                = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.updateGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: UpdateGovTalkStatusRequest = mkUpdateGovTalkStatusRequest()
      val error                                = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.updateGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: UpdateGovTalkStatusRequest = mkUpdateGovTalkStatusRequest()
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.updateGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.updateGovTalkStatus(request).futureValue

      verify(connector, times(1)).updateGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService updateGovTalkStatusCorrelationId" - {

    "must delegate to connector (happy path)" in {
      val connector                                        = mock[FilingFormpProxyConnector]
      val service                                          = new ChrisService(connector)
      val request: UpdateGovTalkStatusCorrelationIdRequest = mkUpdateGovTalkStatusCorrelationIdRequest()
      implicit val hc: HeaderCarrier                       = HeaderCarrier()

      when(connector.updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.updateGovTalkStatusCorrelationId(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                                        = mock[FilingFormpProxyConnector]
      val service                                          = new ChrisService(connector)
      val request: UpdateGovTalkStatusCorrelationIdRequest = mkUpdateGovTalkStatusCorrelationIdRequest()
      val error                                             = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier                       = HeaderCarrier()

      when(connector.updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatusCorrelationId(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                                        = mock[FilingFormpProxyConnector]
      val service                                          = new ChrisService(connector)
      val request: UpdateGovTalkStatusCorrelationIdRequest = mkUpdateGovTalkStatusCorrelationIdRequest()
      val error                                             = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier                       = HeaderCarrier()

      when(connector.updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatusCorrelationId(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                                        = mock[FilingFormpProxyConnector]
      val service                                          = new ChrisService(connector)
      val request: UpdateGovTalkStatusCorrelationIdRequest = mkUpdateGovTalkStatusCorrelationIdRequest()
      implicit val hc: HeaderCarrier                       = HeaderCarrier()

      when(connector.updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.updateGovTalkStatusCorrelationId(request).futureValue

      verify(connector, times(1)).updateGovTalkStatusCorrelationId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService updateGovTalkStatusLock" - {

    "must delegate to connector (happy path)" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatusLockRequest = mkUpdateGovTalkStatusLockRequest()
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.updateGovTalkStatusLock(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatusLockRequest = mkUpdateGovTalkStatusLockRequest()
      val error                                    = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatusLock(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatusLockRequest = mkUpdateGovTalkStatusLockRequest()
      val error                                    = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatusLock(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatusLockRequest = mkUpdateGovTalkStatusLockRequest()
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.updateGovTalkStatusLock(request).futureValue

      verify(connector, times(1)).updateGovTalkStatusLock(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService updateGovTalkStatistics" - {

    "must delegate to connector (happy path)" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatisticsRequest = mkUpdateGovTalkStatisticsRequest()
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.updateGovTalkStatistics(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatisticsRequest = mkUpdateGovTalkStatisticsRequest()
      val error                                    = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatistics(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatisticsRequest = mkUpdateGovTalkStatisticsRequest()
      val error                                    = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.updateGovTalkStatistics(request).failed.futureValue
      ex mustBe error

      verify(connector).updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                               = mock[FilingFormpProxyConnector]
      val service                                 = new ChrisService(connector)
      val request: UpdateGovTalkStatisticsRequest = mkUpdateGovTalkStatisticsRequest()
      implicit val hc: HeaderCarrier              = HeaderCarrier()

      when(connector.updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.updateGovTalkStatistics(request).futureValue

      verify(connector, times(1)).updateGovTalkStatistics(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService deleteGovTalkStatus" - {

    "must delegate to connector (happy path)" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: DeleteGovTalkStatusRequest = mkDeleteGovTalkStatusRequest()
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      val result = service.deleteGovTalkStatus(request).futureValue
      result mustBe mkGovTalkStatusReturn()

      verify(connector).deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: DeleteGovTalkStatusRequest = mkDeleteGovTalkStatusRequest()
      val error                                = UpstreamErrorResponse("Not found", 404)
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.deleteGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: DeleteGovTalkStatusRequest = mkDeleteGovTalkStatusRequest()
      val error                                = new RuntimeException("Connection timeout")
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.deleteGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: DeleteGovTalkStatusRequest = mkDeleteGovTalkStatusRequest()
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkGovTalkStatusReturn()))

      service.deleteGovTalkStatus(request).futureValue

      verify(connector, times(1)).deleteGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService selectGovTalkStatus" - {

    "must delegate to connector (happy path)" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: SelectGovTalkStatusRequest = mkSelectGovTalkStatusRequest()
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.selectGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSelectGovTalkStatusResponse()))

      val result = service.selectGovTalkStatus(request).futureValue
      result mustBe mkSelectGovTalkStatusResponse()

      verify(connector).selectGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: SelectGovTalkStatusRequest = mkSelectGovTalkStatusRequest()
      val error                                = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.selectGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.selectGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).selectGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: SelectGovTalkStatusRequest = mkSelectGovTalkStatusRequest()
      val error                                = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.selectGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.selectGovTalkStatus(request).failed.futureValue
      ex mustBe error

      verify(connector).selectGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                           = mock[FilingFormpProxyConnector]
      val service                             = new ChrisService(connector)
      val request: SelectGovTalkStatusRequest = mkSelectGovTalkStatusRequest()
      implicit val hc: HeaderCarrier          = HeaderCarrier()

      when(connector.selectGovTalkStatus(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSelectGovTalkStatusResponse()))

      service.selectGovTalkStatus(request).futureValue

      verify(connector, times(1)).selectGovTalkStatus(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
  
  "ChrisService selectGovTalkFormResultId" - {

    "must delegate to connector (happy path)" in {
      val connector                                 = mock[FilingFormpProxyConnector]
      val service                                   = new ChrisService(connector)
      val request: SelectGovTalkFormResultIdRequest = mkSelectGovTalkFormResultIdRequest()
      implicit val hc: HeaderCarrier                = HeaderCarrier()

      when(connector.selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSelectGovTalkFormResultIdResponse()))

      val result = service.selectGovTalkFormResultId(request).futureValue
      result mustBe mkSelectGovTalkFormResultIdResponse()

      verify(connector).selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate failures from connector" in {
      val connector                                 = mock[FilingFormpProxyConnector]
      val service                                   = new ChrisService(connector)
      val request: SelectGovTalkFormResultIdRequest = mkSelectGovTalkFormResultIdRequest()
      val error                                      = UpstreamErrorResponse("Service unavailable", 503)
      implicit val hc: HeaderCarrier                = HeaderCarrier()

      when(connector.selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.selectGovTalkFormResultId(request).failed.futureValue
      ex mustBe error

      verify(connector).selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must propagate RuntimeException from connector" in {
      val connector                                 = mock[FilingFormpProxyConnector]
      val service                                   = new ChrisService(connector)
      val request: SelectGovTalkFormResultIdRequest = mkSelectGovTalkFormResultIdRequest()
      val error                                      = new RuntimeException("Connection failed")
      implicit val hc: HeaderCarrier                = HeaderCarrier()

      when(connector.selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.failed(error))

      val ex: Throwable = service.selectGovTalkFormResultId(request).failed.futureValue
      ex mustBe error

      verify(connector).selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }

    "must call connector exactly once per request" in {
      val connector                                 = mock[FilingFormpProxyConnector]
      val service                                   = new ChrisService(connector)
      val request: SelectGovTalkFormResultIdRequest = mkSelectGovTalkFormResultIdRequest()
      implicit val hc: HeaderCarrier                = HeaderCarrier()

      when(connector.selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSelectGovTalkFormResultIdResponse()))

      service.selectGovTalkFormResultId(request).futureValue

      verify(connector, times(1)).selectGovTalkFormResultId(eqTo(request))(any[HeaderCarrier])
      verifyNoMoreInteractions(connector)
    }
  }
}