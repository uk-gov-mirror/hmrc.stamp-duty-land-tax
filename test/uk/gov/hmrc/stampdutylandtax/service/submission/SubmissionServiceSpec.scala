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

package uk.gov.hmrc.stampdutylandtax.service.submission

import base.SpecBase
import connectors.{ChrisConnector, FilingFormpProxyConnector}
import models.email.EmailServiceRequest
import models.filing.*
import models.polling.SubmissionForPolling
import models.submission.*
import org.mockito.{ArgumentCaptor, Mockito}
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.*
import service.filing.ChrisService
import service.submission.*
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}

import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.xml.Elem

final class SubmissionServiceSpec extends SpecBase {

  private given ExecutionContext = ExecutionContext.global
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val storn     = "STORN12345"
  private val returnId  = "100001"
  private val cred      = "CRED-1"
  private val sender    = SenderType.Agent
  private val periodEnd = LocalDate.parse("2026-01-31")
  private val sentMark  = "SENT-MARK"
  private val utrn      = "123456789MA"
  private val submissionId = "SUB-1"

  private def aReturn(
                       stornId: Option[String] = Some(storn),
                       ref: Option[String] = Some(returnId),
                       version: Option[String] = Some("1"),
                       submission: Option[Submission] = None
                     ): FullReturn =
    FullReturn(
      stornId           = stornId,
      returnResourceRef = ref,
      returnInfo        = Some(ReturnInfo(returnID = Some(returnId), version = version)),
      submission        = submission
    )

  private def withStatus(status: String): FullReturn =
    aReturn(submission = Some(Submission(
      submissionID     = Some(submissionId),
      submissionStatus = Some(status)
    )))

  private def completed(utrn: Option[String], receivedIrMark: Option[String]): ChrisResponse.Completed =
    ChrisResponse.Completed(utrn, receivedIrMark, Some("corr"), Some("url"), "<response/>")

  private def acknowledged: ChrisResponse.Acknowledged =
    ChrisResponse.Acknowledged(Some("corr"), Some(10), Some("url"), "<ack/>")

  private def errored(errs: GovTalkError*): ChrisResponse.Errored =
    ChrisResponse.Errored(errs.toSeq, Some("corr"), Some("url"), "<error/>")

  private def transportError(message: String): ChrisResponse.TransportError =
    ChrisResponse.TransportError(message, "<transport-error/>")

  private def selectRow(
                         protocol: Option[String],
                         gatewayUrl: Option[String] = Some("http://chris"),
                         formResultId: Option[String] = Some(returnId)
                       ): SelectGovTalkStatusResponse =
    SelectGovTalkStatusResponse(
      userIdentifier       = Some(storn),
      formResultId         = formResultId,
      correlationId        = Some("corr"),
      formLock             = Some("N"),
      createTimestamp      = Some("2026-01-31 09:15:30"),
      endStateTimestamp    = None,
      lastMessageTimestamp = Some("2026-01-31 09:15:30"),
      numberOfPolls        = Some("0"),
      pollInterval         = Some("0"),
      protocolStatus       = protocol,
      gatewayUrl           = gatewayUrl
    )

  private val departmentalError: GovTalkError =
    GovTalkError(raisedBy = "Department", number = Some("3001"), errorType = "business", text = Some("Invalid STORN"), location = Some("/SDLT/x"))

  private val fatalError: GovTalkError =
    GovTalkError(raisedBy = "Gateway", number = Some("9999"), errorType = "fatal", text = Some("Boom"), location = None)

  private val fatalError2: GovTalkError =
    GovTalkError(raisedBy = "Gateway", number = Some("8888"), errorType = "fatal", text = Some("Boom two"), location = None)

  private val recoverableError: GovTalkError =
    GovTalkError(raisedBy = "Gateway", number = Some("2005"), errorType = "timeOut", text = Some("Transient"), location = None)

  private val recoverable1000: GovTalkError =
    GovTalkError(raisedBy = "Gateway", number = Some("1000"), errorType = "fatal", text = Some("Transient 1000"), location = None)

  private val recoverable3000: GovTalkError =
    GovTalkError(raisedBy = "Gateway", number = Some("3000"), errorType = "fatal", text = Some("Transient 3000"), location = None)

  private val pollSubmissionId = "9600001"
  private val pollRef          = "9200001"
  private val pollCorrId       = "CORR123456789012345678901234"
  private val pollSentIrMark   = "SENTIRMARK=="

  private val pollSubmission: SubmissionForPolling =
    SubmissionForPolling(submissionId = pollSubmissionId, storn = storn, returnResourceRef = pollRef, submissionStatus = "ACCEPTED")

  private val pollFullReturn: FullReturn =
    FullReturn(
      stornId           = Some(storn),
      returnResourceRef = Some(pollRef),
      submission        = Some(Submission(
        submissionID          = Some(pollSubmissionId),
        storn                 = Some(storn),
        submissionStatus      = Some("ACCEPTED"),
        irmarkSent            = Some(pollSentIrMark),
        submissionRequestDate = Some("2026-07-23 10:00:00.000")
      ))
    )

  private def pollRow(protocolStatus: String = "dataPoll",
                      lastMessage: String = "2026-07-23 11:00:00",
                      pollInterval: String = "60",
                      corrId: String = pollCorrId,
                      formLock: String = "N",
                      gateway: Option[String] = Some("http://chris.example/poll")): SelectGovTalkStatusResponse =
    SelectGovTalkStatusResponse(
      userIdentifier       = Some(storn),
      formResultId         = Some(pollRef),
      correlationId        = Some(corrId),
      formLock             = Some(formLock),
      createTimestamp      = Some("2026-07-23 10:00:00"),
      endStateTimestamp    = None,
      lastMessageTimestamp = Some(lastMessage),
      numberOfPolls        = Some("2"),
      pollInterval         = Some(pollInterval),
      protocolStatus       = Some(protocolStatus),
      gatewayUrl           = gateway
    )

  private def pollCompleted(irMark: String): ChrisResponse.Completed =
    ChrisResponse.Completed(
      utrn             = Some(utrn),
      receivedIrMark   = Some(irMark),
      correlationId    = Some(pollCorrId),
      responseEndPoint = Some("http://chris.example/poll"),
      rawXml           = "<response/>"
    )

  private final class Fixtures:
    val envelopeBuilder: GovTalkEnvelopeBuilder     = mock[GovTalkEnvelopeBuilder]
    val validator: SchemaValidator                  = mock[SchemaValidator]
    val connector: ChrisConnector                   = mock[ChrisConnector]
    val audit: SubmissionAuditService               = mock[SubmissionAuditService]
    val chrisService: ChrisService                  = mock[ChrisService]
    val emailService: EmailService                  = mock[EmailService]
    val filingConnector: FilingFormpProxyConnector  = mock[FilingFormpProxyConnector]
    val clock: Clock                                = Clock.fixed(Instant.parse("2026-07-23T12:00:00Z"), ZoneOffset.UTC)

    val service = new SubmissionService(envelopeBuilder, validator, connector, audit, chrisService, emailService, filingConnector, clock)

    when(connector.defaultPath).thenReturn("http://chris/ChRIS/SDLT/Filing/sync/SDLT")
    when(validator.validateSdlt(any[Elem])).thenReturn(Right(()))
    when(envelopeBuilder.submissionRequest(any[Elem], any[String], any[LocalDate], any[SenderType], any[String], any[String]))
      .thenReturn(IrMarkResult(<Envelope/>, sentMark, "SENT-MARK-B32"))

    when(chrisService.lockReturn(any[LockReturnRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(Right(LockReturnResponse(true))))
    when(chrisService.deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(DeleteSubmissionErrorDetailReturn(true)))
    when(chrisService.createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(CreateSubmissionReturn(true, Some(submissionId))))
    when(chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(SelectGovTalkStatusResponse(None, None, None, None, None, None, None, None, None, None, None)))
    when(chrisService.resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.updateGovTalkStatusCorrelationId(any[UpdateGovTalkStatusCorrelationIdRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.insertInitialGovTalkStatus(any[InsertInitialGovTalkStatusRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.deleteGovTalkStatus(any[DeleteGovTalkStatusRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(UpdateSubmissionReturn(true)))
    when(chrisService.updateGovTalkStatusLock(any[UpdateGovTalkStatusLockRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.updateGovTalkStatistics(any[UpdateGovTalkStatisticsRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(GovTalkStatusReturn(true)))
    when(chrisService.createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(CreateSubmissionErrorDetailReturn(true)))
    when(audit.auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier]))
      .thenReturn(Future.unit)
    when(emailService.submitEmailConfirmation(any[FullReturn], any[String], any[Option[String]])(any[HeaderCarrier]))
      .thenReturn(Future.successful(()))
    when(connector.delete(any[Option[String]], any[String])(any[HeaderCarrier]))
      .thenReturn(Future.successful(ChrisDeleteResponse.Deleted(Some("corr"), "<delete-response/>")))
    when(filingConnector.getFullReturn(any[GetReturnByRefRequest])(any[HeaderCarrier]))
      .thenReturn(Future.successful(pollFullReturn))

    def onResponse(resp: ChrisResponse): Unit =
      when(connector.submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier])).thenReturn(Future.successful(resp))

    def onPollResponse(resp: ChrisResponse): Unit =
      when(connector.poll(any[Option[String]], any[String])(any[HeaderCarrier])).thenReturn(Future.successful(resp))

    def submissions: Seq[SubmissionUpdate] =
      val captor = ArgumentCaptor.forClass(classOf[UpdateSubmissionRequest])
      verify(chrisService, atLeastOnce()).updateSubmission(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.submission)

    def statuses: Seq[String] = submissions.flatMap(_.submittableStatus)

    def protocols: Seq[String] =
      val captor = ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusRequest])
      verify(chrisService, atLeastOnce()).updateGovTalkStatus(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.protocolStatus)

    def resetRequests: Seq[ResetGovTalkStatusRequest] =
      val captor = ArgumentCaptor.forClass(classOf[ResetGovTalkStatusRequest])
      verify(chrisService, atLeastOnce()).resetGovTalkStatus(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq

    def lockFlags: Seq[String] =
      val captor = ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusLockRequest])
      verify(chrisService, atLeastOnce()).updateGovTalkStatusLock(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.govTalkStatus.formLockNew)

    def submitUrls: Seq[Option[String]] =
      val captor = ArgumentCaptor.forClass(classOf[Option[String]])
      verify(connector, atLeastOnce()).submit(any[Elem], captor.capture(), any[String], any[Option[String]])(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq

    def submitRefs: Seq[Option[String]] =
      val captor = ArgumentCaptor.forClass(classOf[Option[String]])
      verify(connector, atLeastOnce()).submit(any[Elem], any[Option[String]], any[String], captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq

    def statisticsPollIntervals: Seq[String] =
      val captor = ArgumentCaptor.forClass(classOf[UpdateGovTalkStatisticsRequest])
      verify(chrisService, atLeastOnce()).updateGovTalkStatistics(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.govTalkStatus.pollInterval)

    def neverSubmitted(): Unit =
      verify(connector, never()).submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier])

    def statistics: Seq[GovTalkStatusStatistics] =
      val captor = ArgumentCaptor.forClass(classOf[UpdateGovTalkStatisticsRequest])
      verify(chrisService, atLeastOnce()).updateGovTalkStatistics(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.govTalkStatus)

    def lockRequests: Seq[GovTalkStatusLock] =
      val captor = ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusLockRequest])
      verify(chrisService, atLeastOnce()).updateGovTalkStatusLock(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.govTalkStatus)

    def errorDetails: Seq[SubmissionErrorDetail] =
      val captor = ArgumentCaptor.forClass(classOf[CreateSubmissionErrorDetailRequest])
      verify(chrisService, atLeastOnce()).createSubmissionErrorDetail(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq.map(_.submissionErrorDetails)

    def pollTargets: Seq[Option[String]] =
      val captor = ArgumentCaptor.forClass(classOf[Option[String]])
      verify(connector, atLeastOnce()).poll(captor.capture(), any[String])(any[HeaderCarrier])
      captor.getAllValues.asScala.toSeq

  "SubmissionService.submit requireContext" - {

    "must fail with MissingSubmissionContextException when the credential identifier is blank" in {
      val f = new Fixtures
      f.service.submit(aReturn(), sender, periodEnd, "   ").failed.futureValue mustBe a[MissingSubmissionContextException]
      verify(f.chrisService, never()).lockReturn(any[LockReturnRequest])(any[HeaderCarrier])
    }

    "must fail with MissingSubmissionContextException when stornId is missing" in {
      val f = new Fixtures
      f.service.submit(aReturn(stornId = None), sender, periodEnd, cred).failed.futureValue mustBe a[MissingSubmissionContextException]
    }

    "must fail with MissingSubmissionContextException when the return resource ref is missing" in {
      val f = new Fixtures
      f.service.submit(aReturn(ref = None), sender, periodEnd, cred).failed.futureValue mustBe a[MissingSubmissionContextException]
    }

    "must fail with MissingSubmissionContextException when the return version is missing" in {
      val f = new Fixtures
      f.service.submit(aReturn(version = None), sender, periodEnd, cred).failed.futureValue mustBe a[MissingSubmissionContextException]
    }

    "must treat a blank credential as missing even when the return context is otherwise complete" in {
      val f = new Fixtures
      f.service.submit(aReturn(), sender, periodEnd, "").failed.futureValue mustBe a[MissingSubmissionContextException]
      f.neverSubmitted()
    }
  }

  "SubmissionService.submit return lock" - {

    "must fail with ReturnLockConflictException when the lock is refused" in {
      val f = new Fixtures
      when(f.chrisService.lockReturn(any[LockReturnRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(Left(UpstreamErrorResponse("locked", 423))))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue mustBe a[ReturnLockConflictException]
    }

    "must make no ChRIS submit call when the lock is refused" in {
      val f = new Fixtures
      when(f.chrisService.lockReturn(any[LockReturnRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(Left(UpstreamErrorResponse("locked", 423))))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.neverSubmitted()
    }

    "must lock the return with the storn, resource ref and numeric version" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred)

      val captor = ArgumentCaptor.forClass(classOf[LockReturnRequest])
      verify(f.chrisService, Mockito.timeout(2000)).lockReturn(captor.capture())(any[HeaderCarrier])
      val req = captor.getValue
      req.storn mustBe storn
      req.returnResourceRef mustBe returnId
      req.version mustBe 1
    }

    "must parse a non-default return version into the lock request" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(version = Some("2")), sender, periodEnd, cred)

      val captor = ArgumentCaptor.forClass(classOf[LockReturnRequest])
      verify(f.chrisService, Mockito.timeout(2000)).lockReturn(captor.capture())(any[HeaderCarrier])
      captor.getValue.version mustBe 2
    }
  }

  "SubmissionService.submit existing submission" - {

    "must clear prior error details when the existing submission is in DEPARTMENTAL_ERROR" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("DEPARTMENTAL_ERROR"), sender, periodEnd, cred).futureValue
      verify(f.chrisService).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier])
    }

    "must clear prior error details when the existing submission is in FATAL_ERROR" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("FATAL_ERROR"), sender, periodEnd, cred).futureValue
      verify(f.chrisService).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must clear prior error details when the existing submission is in STARTED" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("STARTED"), sender, periodEnd, cred).futureValue
      verify(f.chrisService).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must clear prior error details for a lower-case status (case-insensitive)" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("fatal_error"), sender, periodEnd, cred).futureValue
      verify(f.chrisService).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must clear prior error details for a whitespace-padded status (trimmed)" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("  FATAL_ERROR  "), sender, periodEnd, cred).futureValue
      verify(f.chrisService).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must neither clear nor create when the existing submission is not re-submittable" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("PENDING"), sender, periodEnd, cred).futureValue
      verify(f.chrisService, never()).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier])
    }

    "must neither clear nor create when the existing submission is ACCEPTED" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(withStatus("ACCEPTED"), sender, periodEnd, cred).futureValue
      verify(f.chrisService, never()).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier])
    }

    "must create a submission when none exists" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService).createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).deleteSubmissionErrorDetail(any[DeleteSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }
  }

  "SubmissionService.submit GovTalk status" - {

    "must reset the row when one already exists" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("initial"))))
      f.onResponse(acknowledged) // ack does not trigger the success-path finalise reset, so only the setup reset happens
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).insertInitialGovTalkStatus(any[InsertInitialGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must send a null end-state timestamp on reset (spec F53 step 7)" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("initial"))))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.resetRequests.head.govTalkStatus.endStateTimestamp mustBe None
    }

    "must discard the stored gateway URL on reset and submit to the configured ChRIS URL" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("initial"), gatewayUrl = Some("http://stored-gateway"))))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.submitUrls must contain(None)
      f.submitUrls must not contain Some("http://stored-gateway")
    }

    "must insert an initial row when the lookup fails" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("not found")))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService).insertInitialGovTalkStatus(any[InsertInitialGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must insert an initial row when the existing lookup has a blank formResultId" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("initial"), formResultId = Some("   "))))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService).insertInitialGovTalkStatus(any[InsertInitialGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must delete then re-insert when the row exists but the protocolStatus is empty" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = None)))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService).deleteGovTalkStatus(any[DeleteGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.chrisService).insertInitialGovTalkStatus(any[InsertInitialGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.chrisService, never()).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must fail the whole submit when the delete succeeds but the re-insert fails" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = None)))
      when(f.chrisService.insertInitialGovTalkStatus(any[InsertInitialGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("insert failed")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue mustBe a[RuntimeException]
      verify(f.chrisService).deleteGovTalkStatus(any[DeleteGovTalkStatusRequest])(any[HeaderCarrier])
      f.neverSubmitted()
    }
  }

  "SubmissionService.submit build and validate" - {

    "must fail with SchemaValidationException when validation fails" in {
      val f = new Fixtures
      when(f.validator.validateSdlt(any[Elem])).thenReturn(Left(Seq("bad element", "missing field")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue mustBe a[SchemaValidationException]
    }

    "must build no envelope and make no ChRIS call when validation fails" in {
      val f = new Fixtures
      when(f.validator.validateSdlt(any[Elem])).thenReturn(Left(Seq("bad element")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      verify(f.envelopeBuilder, never()).submissionRequest(any[Elem], any[String], any[LocalDate], any[SenderType], any[String], any[String])
      f.neverSubmitted()
    }

    "must persist a PENDING seed carrying the sent IRmark" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      val seed = f.submissions.head
      seed.submittableStatus mustBe Some("PENDING")
      seed.IRMarkSent        mustBe Some(sentMark)
    }

    "must build the envelope with the storn, period end, sender and credential" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.envelopeBuilder)
        .submissionRequest(any[Elem], eqTo(storn), eqTo(periodEnd), eqTo(sender), eqTo(cred), any[String])
    }
  }

  "SubmissionService.submit GovTalk lock gate" - {

    "must fail with GovTalkLockNotAcquiredException when the lock cannot be acquired" in {
      val f = new Fixtures
      when(f.chrisService.updateGovTalkStatusLock(any[UpdateGovTalkStatusLockRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("lock held")))
      val ex = f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      ex mustBe a[GovTalkLockNotAcquiredException]
      ex.asInstanceOf[GovTalkLockNotAcquiredException].formResultId mustBe submissionId
    }

    "must make no ChRIS call when the lock cannot be acquired" in {
      val f = new Fixtures
      when(f.chrisService.updateGovTalkStatusLock(any[UpdateGovTalkStatusLockRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("lock held")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.neverSubmitted()
    }

    "must submit to ChRIS once when the lock is acquired" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.connector).submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier])
    }

    "must forward the return resource ref to the ChRIS submit call" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.submitRefs must contain(Some(returnId))
    }
  }

  "SubmissionService.submit on Completed with a matching IRmark" - {

    def run(f: Fixtures) =
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred)

    "must resolve to a SUBMITTED outcome carrying the UTRN" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      val outcome = f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      outcome.status mustBe UniversalStatus.SUBMITTED
      outcome.utrn   mustBe Some(utrn)
    }

    "must resolve the status to SUBMITTED" in {
      val f = new Fixtures; run(f).futureValue
      f.statuses must contain("SUBMITTED")
    }

    "must preserve the sent IRmark on the UTRN write (accumulator not nulled)" in {
      val f = new Fixtures; run(f).futureValue
      f.submissions.find(_.utrn.isDefined).value.IRMarkSent mustBe Some(sentMark)
    }

    "must record the UTRN and received IRmark" in {
      val f = new Fixtures; run(f).futureValue
      val w = f.submissions.find(_.utrn.isDefined).value
      w.utrn           mustBe Some(utrn)
      w.IRMarkRecieved mustBe Some(sentMark)
    }

    "must set the acceptedDate on a successful submission" in {
      val f = new Fixtures; run(f).futureValue
      f.submissions.exists(_.acceptedDate.isDefined) mustBe true
    }

    "must drive the GovTalk deleteRequest and endState protocol transitions" in {
      val f = new Fixtures; run(f).futureValue
      f.protocols must contain allOf ("deleteRequest", "endState")
    }

    "must reset the GovTalk status after endState on a successful delete" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must neither set endState nor reset when the ChRIS delete is unsuccessful" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      when(f.connector.delete(any[Option[String]], any[String])(any[HeaderCarrier]))
        .thenReturn(Future.successful(ChrisDeleteResponse.TransportError("delete boom", "<x/>")))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService, never()).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
      f.protocols mustNot contain("endState")
    }

    "must update the GovTalk statistics" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService).updateGovTalkStatistics(any[UpdateGovTalkStatisticsRequest])(any[HeaderCarrier])
    }

    "must send the ChRIS delete for the response resource" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.connector).delete(any[Option[String]], any[String])(any[HeaderCarrier])
    }

    "must audit the submission" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }

    "must not create any error details" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService, never()).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must run the datetime footer" in {
      val f = new Fixtures; run(f).futureValue
      f.submissions.exists(_.submissionRequestDate.isDefined) mustBe true
    }
  }

  "SubmissionService.submit on Completed ChRIS delete resilience" - {

    "must still resolve SUBMITTED and audit when the ChRIS delete fails" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      when(f.connector.delete(any[Option[String]], any[String])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("delete down")))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("SUBMITTED")
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }
  }

  "SubmissionService.submit on Completed with a non-matching IRmark" - {

    "must resolve the status to SUBMITTED_NO_RECEIPT" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some("DIFFERENT-MARK")))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("SUBMITTED_NO_RECEIPT")
    }

    "must still record the UTRN and audit" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some("DIFFERENT-MARK")))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.submissions.exists(_.utrn.contains(utrn)) mustBe true
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }
  }

  "SubmissionService.submit on Acknowledged" - {

    def run(f: Fixtures) =
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred)

    "must resolve to an ACCEPTED outcome with no UTRN" in {
      val f = new Fixtures
      val outcome = run(f).futureValue
      outcome.status mustBe UniversalStatus.ACCEPTED
      outcome.utrn   mustBe None
    }

    "must resolve the status to ACCEPTED" in {
      val f = new Fixtures; run(f).futureValue
      f.statuses must contain("ACCEPTED")
    }

    "must set the acceptedDate" in {
      val f = new Fixtures; run(f).futureValue
      f.submissions.exists(_.acceptedDate.isDefined) mustBe true
    }

    "must drive the dataPoll protocol transition" in {
      val f = new Fixtures; run(f).futureValue
      f.protocols must contain("dataPoll")
    }

    "must update the GovTalk statistics" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService).updateGovTalkStatistics(any[UpdateGovTalkStatisticsRequest])(any[HeaderCarrier])
    }

    "must propagate the poll interval into the GovTalk statistics" in {
      val f = new Fixtures; run(f).futureValue
      f.statisticsPollIntervals must contain("10")
    }

    "must not audit" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.audit, never()).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }

    "must not create any error details" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService, never()).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }
  }

  "SubmissionService.submit on a departmental (business-reject) error" - {

    def run(f: Fixtures) =
      f.onResponse(errored(departmentalError))
      f.service.submit(aReturn(), sender, periodEnd, cred)

    "must resolve the status to DEPARTMENTAL_ERROR" in {
      val f = new Fixtures; run(f).futureValue
      f.statuses must contain("DEPARTMENTAL_ERROR")
    }

    "must record the first error's code, classified type and message" in {
      val f = new Fixtures; run(f).futureValue
      val w = f.submissions.find(_.govTalkErrorCode.isDefined).value
      w.govTalkErrorCode    mustBe Some("3001")
      w.govTalkErrorType    mustBe Some("departmentalError")
      w.govTalkErrorMessage mustBe Some("Invalid STORN")
    }

    "must drive the GovTalk deleteRequest and endState protocol transitions" in {
      val f = new Fixtures; run(f).futureValue
      f.protocols must contain allOf ("deleteRequest", "endState")
    }

    "must create an error detail" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must audit the submission" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }
  }

  "SubmissionService.submit on a fatal error" - {

    def run(f: Fixtures) =
      f.onResponse(errored(fatalError))
      f.service.submit(aReturn(), sender, periodEnd, cred)

    "must resolve the status to FATAL_ERROR" in {
      val f = new Fixtures; run(f).futureValue
      f.statuses must contain("FATAL_ERROR")
    }

    "must not drive any GovTalk protocol transition" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService, never()).updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must still create an error detail and audit" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }

    "must run the datetime footer" in {
      val f = new Fixtures; run(f).futureValue
      f.submissions.exists(_.submissionRequestDate.isDefined) mustBe true
    }
  }

  "SubmissionService.submit on a recoverable error" - {

    def run(f: Fixtures) =
      f.onResponse(errored(recoverableError))
      f.service.submit(aReturn(), sender, periodEnd, cred)

    "must overwrite the status to STARTED" in {
      val f = new Fixtures; run(f).futureValue
      f.statuses must contain("STARTED")
    }

    "must clear the request datetime and skip the footer" in {
      val f = new Fixtures; run(f).futureValue
      f.submissions.forall(_.submissionRequestDate.isEmpty) mustBe true
    }

    "must not drive any GovTalk protocol transition" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService, never()).updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must still create an error detail even though the error is recoverable" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.chrisService).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must audit the submission" in {
      val f = new Fixtures; run(f).futureValue
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }

    "must keep the original submission request date when a return is submitted again" in {
      val f = new Fixtures
      val existing = aReturn(submission = Some(Submission(
        submissionID          = Some("SUB-1"),
        storn                 = Some(storn),
        submissionStatus      = Some("ACCEPTED"),
        submissionRequestDate = Some("2026-07-23 10:00:00.000")
      )))
      f.onResponse(completed(Some("UTRN123"), None))

      f.service.submit(existing, sender, periodEnd, cred).futureValue

      f.submissions.head.submissionRequestDate mustBe Some("2026-07-23 10:00:00.000")
    }

    "must overwrite the status to STARTED for recoverable error 1000" in {
      val f = new Fixtures
      f.onResponse(errored(recoverable1000))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("STARTED")
    }

    "must overwrite the status to STARTED for recoverable error 3000" in {
      val f = new Fixtures
      f.onResponse(errored(recoverable3000))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("STARTED")
    }

    "must overwrite the status to STARTED when a departmental rejection also carries a recoverable code" in {
      val f = new Fixtures
      f.onResponse(errored(departmentalError, recoverable1000))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("DEPARTMENTAL_ERROR")
      f.statuses must contain("STARTED")
    }
  }

  "SubmissionService.submit on a TransportError" - {

    "must resolve a non-timeout transport error to FATAL_ERROR" in {
      val f = new Fixtures
      f.onResponse(transportError("connection reset"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("FATAL_ERROR")
    }

    "must create no error details and set no error fields for a transport error (no GovTalk errors)" in {
      val f = new Fixtures
      f.onResponse(transportError("connection reset"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService, never()).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
      f.submissions.forall(_.govTalkErrorCode.isEmpty) mustBe true
    }

    "must still audit a transport error" in {
      val f = new Fixtures
      f.onResponse(transportError("connection reset"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }

    "must resolve a timeout transport error to STARTED" in {
      val f = new Fixtures
      f.onResponse(transportError("Read timed out"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses must contain("STARTED")
    }

    "must not drive any GovTalk protocol transition on a transport error" in {
      val f = new Fixtures
      f.onResponse(transportError("connection reset"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService, never()).updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must still audit a timeout transport error" in {
      val f = new Fixtures
      f.onResponse(transportError("Read timed out"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }
  }

  "SubmissionService.submit on multiple GovTalk errors" - {

    "must create one error detail per error" in {
      val f = new Fixtures
      f.onResponse(errored(fatalError, fatalError2))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      verify(f.chrisService, times(2)).createSubmissionErrorDetail(any[CreateSubmissionErrorDetailRequest])(any[HeaderCarrier])
    }

    "must not attempt to release the GovTalk lock when the row has already been unlocked by the reset" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("dataPoll"))))
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[UpdateGovTalkStatusLockRequest] =
        ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusLockRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatusLock(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.map(_.govTalkStatus.formLockNew) must not contain "N"
    }

    "must preserve the poll interval and gateway url stored by the acknowledgement when releasing the lock" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("dataPoll"), gatewayUrl = Some("http://chris.example/poll/abc"))
          .copy(pollInterval = Some("120"), formLock = Some("Y"))))
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[UpdateGovTalkStatusLockRequest] =
        ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusLockRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatusLock(captor.capture())(any[HeaderCarrier])
      val release = captor.getAllValues.asScala.toList.map(_.govTalkStatus).filter(_.formLockNew == "N")

      release.map(_.pollInterval) must contain("120")
      release.map(_.gatewayUrl) must contain("http://chris.example/poll/abc")
    }

    "must store the new correlation id after resetting an existing GovTalk row" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("endState"))))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[UpdateGovTalkStatusCorrelationIdRequest] =
        ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusCorrelationIdRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatusCorrelationId(captor.capture())(any[HeaderCarrier])
      val first = captor.getAllValues.get(0)
      first.correlationId must not be "empty"
      first.correlationId.length mustBe 32
    }

    "must fail the submit without sending to ChRIS when the correlation id cannot be stored after a reset" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("endState"))))
      when(f.chrisService.updateGovTalkStatusCorrelationId(any[UpdateGovTalkStatusCorrelationIdRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("correlation id write failed", 500)))
      f.onResponse(acknowledged)

      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue mustBe a[UpstreamErrorResponse]
      verify(f.connector, never()).submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier])
    }

    "must not mark the GovTalk row pollable when the id ChRIS returned cannot be stored" in {
      val f = new Fixtures
      when(f.chrisService.updateGovTalkStatusCorrelationId(any[UpdateGovTalkStatusCorrelationIdRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("correlation id write failed", 500)))
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      verify(f.chrisService, never()).updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier])
    }

    "must mark the GovTalk row pollable when the id ChRIS returned is stored" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[UpdateGovTalkStatusRequest] =
        ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatus(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.map(_.protocolStatus) must contain("dataPoll")
    }

    "must still complete the submission when the id ChRIS returned cannot be stored" in {
      val f = new Fixtures
      when(f.chrisService.updateGovTalkStatusCorrelationId(any[UpdateGovTalkStatusCorrelationIdRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("correlation id write failed", 500)))
      f.onResponse(acknowledged)

      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue.status mustBe UniversalStatus.ACCEPTED
    }

    "must overwrite the stored correlation id with the one ChRIS returns" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[UpdateGovTalkStatusCorrelationIdRequest] =
        ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusCorrelationIdRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatusCorrelationId(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.map(_.correlationId).last mustBe "corr"
    }

    "must keep the id we generated when ChRIS returns no correlation id" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(selectRow(protocol = Some("endState"))))
      f.onResponse(ChrisResponse.Acknowledged(None, Some(10), Some("url"), "<ack/>"))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[UpdateGovTalkStatusCorrelationIdRequest] =
        ArgumentCaptor.forClass(classOf[UpdateGovTalkStatusCorrelationIdRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatusCorrelationId(captor.capture())(any[HeaderCarrier])
      captor.getAllValues.asScala.map(_.correlationId).distinct.size mustBe 1
      captor.getAllValues.get(0).correlationId.length mustBe 32
    }

    "must send the ChRIS delete with the correlation id ChRIS returned" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
      verify(f.connector, atLeastOnce()).delete(any[Option[String]], captor.capture())(any[HeaderCarrier])
      captor.getValue mustBe "corr"
    }

    "must record the fields of the first error only" in {
      val f = new Fixtures
      f.onResponse(errored(fatalError, fatalError2))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.submissions.find(_.govTalkErrorCode.isDefined).value.govTalkErrorCode mustBe Some("9999")
    }

    "must number each error detail from zero and prefix the message with the error code" in {
      val f = new Fixtures
      f.onResponse(errored(fatalError, fatalError2))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue

      val captor: ArgumentCaptor[CreateSubmissionErrorDetailRequest] =
        ArgumentCaptor.forClass(classOf[CreateSubmissionErrorDetailRequest])
      verify(f.chrisService, times(2)).createSubmissionErrorDetail(captor.capture())(any[HeaderCarrier])
      val details = captor.getAllValues.asScala.toList.map(_.submissionErrorDetails)

      details.map(_.position) mustBe List("0", "1")
      details.map(_.errorMessage) mustBe List("9999: Boom", "8888: Boom two")
    }
  }

  "SubmissionService.submit lock lifecycle" - {

    "must release the GovTalk lock after a successful response" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.lockFlags must contain("N")
    }

    "must acquire the GovTalk lock before releasing it" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.lockFlags must contain allOf ("Y", "N")
    }

    "must release the GovTalk lock after an error response" in {
      val f = new Fixtures
      f.onResponse(errored(fatalError))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.lockFlags must contain("N")
    }

    "must release the GovTalk lock even when the ChRIS submit fails" in {
      val f = new Fixtures
      when(f.connector.submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("chris down")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.lockFlags must contain("N")
    }
  }

  "SubmissionService.submit cleanup on failure" - {

    "must fail and write no status beyond PENDING when the ChRIS submit fails" in {
      val f = new Fixtures
      when(f.connector.submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("chris down")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.statuses.toSet mustBe Set("PENDING")
      f.submissions.forall(_.utrn.isEmpty) mustBe true
    }

    "must not stamp a submission-request datetime when the ChRIS submit fails" in {
      val f = new Fixtures
      when(f.connector.submit(any[Elem], any[Option[String]], any[String], any[Option[String]])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("chris down")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.submissions.forall(_.submissionRequestDate.isEmpty) mustBe true
    }

    "must propagate the failure and still release the lock when auditing fails" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      when(f.audit.auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("audit down")))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.lockFlags must contain("N")
    }
  }

  "SubmissionService.submit status is never clobbered by the datetime footer" - {

    "must stamp the datetime footer onto an ACCEPTED status, not revert it to PENDING" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      val footerWrite = f.submissions.find(_.submissionRequestDate.isDefined).value
      footerWrite.submittableStatus mustBe Some("ACCEPTED")
    }

    "must leave ACCEPTED as the final persisted status on an Acknowledged response" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses.lastOption mustBe Some("ACCEPTED")
    }

    "must stamp the datetime footer onto a SUBMITTED status carrying the UTRN, not revert it to PENDING" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      val footerWrite = f.submissions.find(_.submissionRequestDate.isDefined).value
      footerWrite.submittableStatus mustBe Some("SUBMITTED")
      footerWrite.utrn              mustBe Some(utrn)
    }

    "must leave SUBMITTED as the final persisted status on a Completed response" in {
      val f = new Fixtures
      f.onResponse(completed(Some(utrn), Some(sentMark)))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      f.statuses.lastOption mustBe Some("SUBMITTED")
    }

    "must stamp the datetime footer onto a FATAL_ERROR status, not revert it to PENDING" in {
      val f = new Fixtures
      f.onResponse(errored(fatalError))
      f.service.submit(aReturn(), sender, periodEnd, cred).futureValue
      val footerWrite = f.submissions.find(_.submissionRequestDate.isDefined).value
      footerWrite.submittableStatus mustBe Some("FATAL_ERROR")
    }

    "must not revert an already-ACCEPTED status to PENDING when the post-accept GovTalk update fails" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      when(f.chrisService.updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("updateGovTalkStatus failed: 500", 500)))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.statuses.lastOption mustBe Some("ACCEPTED")
    }

    "must skip the datetime footer entirely when the submission fails after the status was persisted" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      when(f.chrisService.updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("updateGovTalkStatus failed: 500", 500)))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.submissions.forall(_.submissionRequestDate.isEmpty) mustBe true
    }

    "must still release the GovTalk lock when the post-accept GovTalk update fails" in {
      val f = new Fixtures
      f.onResponse(acknowledged)
      when(f.chrisService.updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("updateGovTalkStatus failed: 500", 500)))
      f.service.submit(aReturn(), sender, periodEnd, cred).failed.futureValue
      f.lockFlags must contain("N")
    }
  }

  "SubmissionService.poll choosing whether to poll" - {

    "skips a submission whose GovTalk status is not at dataPoll" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(protocolStatus = "initial")))

      f.service.poll(pollSubmission).futureValue.polled mustBe false
      verify(f.connector, never()).poll(any[Option[String]], any[String])(any[HeaderCarrier])
    }

    "skips a submission whose poll interval has not yet elapsed" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(lastMessage = "2026-07-23 11:59:30", pollInterval = "3600")))

      f.service.poll(pollSubmission).futureValue.polled mustBe false
      verify(f.connector, never()).poll(any[Option[String]], any[String])(any[HeaderCarrier])
    }

    "skips a submission with no correlation id on its GovTalk status" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(corrId = "empty")))

      f.service.poll(pollSubmission).futureValue.polled mustBe false
      verify(f.connector, never()).poll(any[Option[String]], any[String])(any[HeaderCarrier])
    }
  }

  "SubmissionService.poll holding the GovTalk row lock" - {

    "skips a submission when the GovTalk row lock cannot be acquired" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      when(f.chrisService.updateGovTalkStatusLock(any[UpdateGovTalkStatusLockRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("already locked")))

      f.service.poll(pollSubmission).futureValue.polled mustBe false
      verify(f.connector, never()).poll(any[Option[String]], any[String])(any[HeaderCarrier])
    }

    "skips a submission when formp rejects the lock because the row is already held" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      when(f.chrisService.updateGovTalkStatusLock(any[UpdateGovTalkStatusLockRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(UpstreamErrorResponse("row already locked", 500)))

      f.service.poll(pollSubmission).futureValue.polled mustBe false
      verify(f.connector, never()).poll(any[Option[String]], any[String])(any[HeaderCarrier])
      verify(f.chrisService, never()).updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier])
    }

    "releases the GovTalk row lock with the freshest interval and gateway after an acknowledgement" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(
          Future.successful(pollRow()),
          Future.successful(pollRow(formLock = "Y", pollInterval = "120").copy(gatewayUrl = Some("http://chris.example/next")))
        )
      f.onPollResponse(ChrisResponse.Acknowledged(Some(pollCorrId), Some(120), Some("http://chris.example/next"), "<ack/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.lockRequests.filter(_.formLockNew == "N").map(_.pollInterval) must contain("120")
      f.lockRequests.filter(_.formLockNew == "N").map(_.gatewayUrl)   must contain("http://chris.example/next")
    }

    "releases the GovTalk row lock with the values read before the poll when the status cannot be re-read" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(pollInterval = "300", gateway = Some("http://chris.example/poll"))))
        .thenReturn(Future.failed(new RuntimeException("formp unavailable")))
      f.onPollResponse(pollCompleted(pollSentIrMark))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      val releases = f.lockRequests.filter(_.formLockNew == "N")
      releases.map(_.pollInterval) must contain only "300"
      releases.map(_.gatewayUrl)   must contain only "http://chris.example/poll"
    }

    "does not attempt to release the GovTalk row lock when the reset has already unlocked it" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(
          Future.successful(pollRow()),
          Future.successful(pollRow(formLock = "N"))
        )
      f.onPollResponse(pollCompleted(pollSentIrMark))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.lockRequests.filter(_.formLockNew == "N") mustBe empty
    }

    "trims the stored poll interval before sending it to formp-proxy" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(pollInterval = " 60 ")))
      f.onPollResponse(ChrisResponse.Acknowledged(Some(pollCorrId), None, None, "<ack/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.lockRequests.map(_.pollInterval) must contain only "60"
      f.statistics.map(_.pollInterval) must contain only "60"
    }
  }

  "SubmissionService.poll deciding where to send the poll" - {

    "polls the stored gateway url when it already carries the ChRIS path" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(gateway = Some("http://chris/ChRIS/SDLT/Filing/sync/SDLT"))))
      f.onPollResponse(ChrisResponse.Acknowledged(Some(pollCorrId), Some(60), None, "<ack/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.pollTargets mustBe Seq(Some("http://chris/ChRIS/SDLT/Filing/sync/SDLT"))
    }

    "keys GovTalk by the submission id and the submission by the return resource ref" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(pollCompleted(pollSentIrMark))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      val statsKeys = ArgumentCaptor.forClass(classOf[UpdateGovTalkStatisticsRequest])
      verify(f.chrisService, atLeastOnce()).updateGovTalkStatistics(statsKeys.capture())(any[HeaderCarrier])
      statsKeys.getAllValues.asScala.toSeq.map(_.formResultId).distinct mustBe Seq(pollSubmissionId)

      val submissionKeys = ArgumentCaptor.forClass(classOf[UpdateSubmissionRequest])
      verify(f.chrisService, atLeastOnce()).updateSubmission(submissionKeys.capture())(any[HeaderCarrier])
      submissionKeys.getAllValues.asScala.toSeq.map(_.returnResourceRef).distinct mustBe Seq(pollRef)
    }

    "falls back to the full ChRIS submission url when no gateway url is stored or returned" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(gateway = None)))
      f.onPollResponse(ChrisResponse.Acknowledged(Some(pollCorrId), Some(60), None, "<ack/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statistics.map(_.gatewayUrl) must contain("http://chris/ChRIS/SDLT/Filing/sync/SDLT")
    }
  }

  "SubmissionService.poll recording the attempt before it is sent" - {

    "increments the number of polls before the poll is sent" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(pollCompleted(pollSentIrMark))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      val ordered = Mockito.inOrder(f.chrisService, f.connector)
      ordered.verify(f.chrisService).updateGovTalkStatistics(any[UpdateGovTalkStatisticsRequest])(any[HeaderCarrier])
      ordered.verify(f.connector).poll(any[Option[String]], any[String])(any[HeaderCarrier])
      f.statistics.head.numberOfPolls mustBe "3"
    }
  }

  "SubmissionService.poll on Acknowledged" - {

    "leaves the submission ACCEPTED and stores the new poll interval and gateway when ChRIS acknowledges again" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(ChrisResponse.Acknowledged(Some(pollCorrId), Some(120), Some("http://chris.example/next"), "<ack/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("ACCEPTED")
      f.statistics.map(_.pollInterval) must contain("120")
      f.statistics.map(_.gatewayUrl) must contain("http://chris.example/next")
    }
  }

  "SubmissionService.poll on Completed" - {

    "marks the submission SUBMITTED and completes the delete, reset, audit and email steps when the poll completes with a matching IR mark" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(pollCompleted(pollSentIrMark))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("SUBMITTED")
      verify(f.connector).delete(any[Option[String]], any[String])(any[HeaderCarrier])
      verify(f.chrisService).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.audit).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
      verify(f.emailService).submitEmailConfirmation(any[FullReturn], any[String], any[Option[String]])(any[HeaderCarrier])
    }

    "marks the submission SUBMITTED_NO_RECEIPT when the received IR mark does not match" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(pollCompleted("DIFFERENTMARK=="))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("SUBMITTED_NO_RECEIPT")
    }

    "leaves GovTalk at deleteRequest when the ChRIS delete fails after a successful poll" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(pollCompleted(pollSentIrMark))
      when(f.connector.delete(any[Option[String]], any[String])(any[HeaderCarrier]))
        .thenReturn(Future.successful(ChrisDeleteResponse.TransportError("client timeout")))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("SUBMITTED")
      f.protocols mustBe Seq("deleteRequest")
      verify(f.chrisService, never()).resetGovTalkStatus(any[ResetGovTalkStatusRequest])(any[HeaderCarrier])
      verify(f.emailService).submitEmailConfirmation(any[FullReturn], any[String], any[Option[String]])(any[HeaderCarrier])
    }

    "still sends the confirmation email and persists the UTRN when the CIP audit fails" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(pollCompleted(pollSentIrMark))
      when(f.audit.auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("datastream down")))

      f.service.poll(pollSubmission).futureValue

      f.submissions.flatMap(_.utrn)           must contain(utrn)
      f.submissions.flatMap(_.IRMarkRecieved) must contain(pollSentIrMark)
      verify(f.emailService).submitEmailConfirmation(any[FullReturn], any[String], any[Option[String]])(any[HeaderCarrier])
    }

    "completes the submission and still reports it as polled when ChRIS returns no UTRN" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(ChrisResponse.Completed(None, Some(pollSentIrMark), Some(pollCorrId), None, "<ok/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("SUBMITTED")
      f.submissions.flatMap(_.utrn) mustBe empty
      verify(f.connector).delete(any[Option[String]], any[String])(any[HeaderCarrier])
    }
  }

  "SubmissionService.poll on Errored" - {

    "marks the submission DEPARTMENTAL_ERROR and records error details on a departmental error" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      val departmental = GovTalkError(raisedBy = "Department", number = Some("3001"), errorType = "business", text = Some("BVR failure"), location = Some("line 1"))
      f.onPollResponse(ChrisResponse.Errored(Seq(departmental), Some(pollCorrId), None, "<error/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("DEPARTMENTAL_ERROR")
      f.errorDetails.map(_.position)     mustBe Seq("0")
      f.errorDetails.map(_.errorMessage) mustBe Seq("3001: BVR failure")
      verify(f.connector).delete(any[Option[String]], any[String])(any[HeaderCarrier])
    }

    "persists DEPARTMENTAL_ERROR before sending the ChRIS delete" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      val departmental = GovTalkError(raisedBy = "Department", number = Some("3001"), errorType = "business", text = Some("BVR failure"), location = None)
      f.onPollResponse(ChrisResponse.Errored(Seq(departmental), Some(pollCorrId), None, "<error/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      val ordered = Mockito.inOrder(f.chrisService, f.connector)
      ordered.verify(f.chrisService).updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier])
      ordered.verify(f.connector).delete(any[Option[String]], any[String])(any[HeaderCarrier])
    }

    "marks the submission FATAL_ERROR and sends no delete on a fatal gateway error" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      val fatal = GovTalkError(raisedBy = "Gateway", number = Some("2000"), errorType = "fatal", text = Some("schema failure"), location = None)
      f.onPollResponse(ChrisResponse.Errored(Seq(fatal), Some(pollCorrId), None, "<error/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("FATAL_ERROR")
      verify(f.connector, never()).delete(any[Option[String]], any[String])(any[HeaderCarrier])
      verify(f.chrisService, never()).updateGovTalkStatus(any[UpdateGovTalkStatusRequest])(any[HeaderCarrier])
      f.errorDetails.map(_.errorMessage) mustBe Seq("2000: schema failure")
    }

    "resets the submission to STARTED when the error is recoverable" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      val recoverable = GovTalkError(raisedBy = "Gateway", number = Some("1000"), errorType = "fatal", text = Some("try again"), location = None)
      f.onPollResponse(ChrisResponse.Errored(Seq(recoverable), Some(pollCorrId), None, "<error/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses.head mustBe "FATAL_ERROR"
      f.statuses.last mustBe "STARTED"
      f.submissions.last.submissionRequestDate mustBe None
    }

    "resets a departmental error to STARTED when the response carries a recoverable code" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      val departmental = GovTalkError(raisedBy = "Department", number = Some("3001"), errorType = "business", text = Some("BVR failure"), location = None)
      val recoverable  = GovTalkError(raisedBy = "Gateway", number = Some("1000"), errorType = "fatal", text = Some("try again"), location = None)
      f.onPollResponse(ChrisResponse.Errored(Seq(departmental, recoverable), Some(pollCorrId), None, "<error/>"))

      f.service.poll(pollSubmission).futureValue.polled mustBe true

      f.statuses must contain("DEPARTMENTAL_ERROR")
      f.statuses.last mustBe "STARTED"
      f.submissions.last.submissionRequestDate mustBe None
    }
  }

  "SubmissionService.poll on a TransportError" - {

    "leaves the submission untouched and does not count it as polled when ChRIS gives no response" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(ChrisResponse.TransportError("client timeout"))

      f.service.poll(pollSubmission).futureValue.polled mustBe false

      verify(f.chrisService, never()).updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier])
      verify(f.audit, never()).auditSubmission(any[String], any[String], any[String], any[FullReturn], any[ChrisResponse])(any[HeaderCarrier])
    }

    "reports a dash in both result columns when ChRIS gives no response" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow()))
      f.onPollResponse(ChrisResponse.TransportError("client timeout"))

      val outcome = f.service.poll(pollSubmission).futureValue
      outcome.pollResult      mustBe "-"
      outcome.newReturnStatus mustBe "-"
      outcome.correlationId   mustBe "(not polled)"
    }

    "reports a dash in both result columns when the submission is skipped before any poll is sent" in {
      val f = new Fixtures
      when(f.chrisService.selectGovTalkStatus(any[SelectGovTalkStatusRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollRow(protocolStatus = "initial")))

      val outcome = f.service.poll(pollSubmission).futureValue
      outcome.pollResult      mustBe "-"
      outcome.newReturnStatus mustBe "-"
      outcome.correlationId   mustBe "(not polled)"
    }
  }

}