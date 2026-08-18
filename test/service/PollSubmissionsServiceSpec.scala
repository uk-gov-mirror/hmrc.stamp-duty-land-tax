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

package service

import connectors.FormpProxyConnector
import models.polling.{SubmissionForPolling, SubmissionsForPollingResponse}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{never, verify, when}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import scheduler.ScheduleStatus.{FailedToPollSubmissions, MongoUnlockException}
import service.submission.SubmissionService
import uk.gov.hmrc.mongo.lock.{Lock, MongoLockRepository}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.{ExecutionContext, Future}

class PollSubmissionsServiceSpec extends AnyFreeSpec with Matchers with ScalaFutures with MockitoSugar {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private val fixedClock: Clock = Clock.fixed(Instant.parse("2026-07-23T12:00:00Z"), ZoneOffset.UTC)

  private val storn = "STN800"

  private def submissionFor(ref: String, id: String): SubmissionForPolling =
    SubmissionForPolling(submissionId = id, storn = storn, returnResourceRef = ref, submissionStatus = "ACCEPTED")

  private val submission = submissionFor("9200001", "9600001")

  private def outcome(sub: SubmissionForPolling, polled: Boolean, pollResult: String = "SUBMITTED"): PollOutcome =
    PollOutcome(sub, polled = polled, pollResult = pollResult, newReturnStatus = pollResult, correlationId = "CORR")

  private trait Setup {
    val mockFormpConnector: FormpProxyConnector = mock[FormpProxyConnector]
    val mockSubmissionService: SubmissionService = mock[SubmissionService]
    val mockServicesConfig: ServicesConfig = mock[ServicesConfig]
    val mockLockRepository: MongoLockRepository = mock[MongoLockRepository]

    when(mockServicesConfig.getString(any())).thenReturn("20 minutes")
    when(mockLockRepository.takeLock(any(), any(), any()))
      .thenReturn(Future.successful(Some(Lock("lockId", "owner", Instant.now(), Instant.now().plusSeconds(1200)))))
    when(mockLockRepository.releaseLock(any(), any())).thenReturn(Future.unit)

    val service: PollSubmissionsService = new PollSubmissionsService(
      mockFormpConnector,
      mockSubmissionService,
      mockServicesConfig,
      mockLockRepository,
      fixedClock
    )

    def worklist(subs: SubmissionForPolling*): Unit =
      when(mockFormpConnector.getSubmissionsForPolling()(any()))
        .thenReturn(Future.successful(SubmissionsForPollingResponse(subs.toList)))
  }

  "PollSubmissionsService" - {

    "choosing which submissions to poll" - {

      "returns an empty list when nothing is due for polling" in new Setup {
        worklist()

        service.invoke.futureValue mustBe Right(Nil)
        verify(mockSubmissionService, never()).poll(any())(any())
      }

      "delegates each submission due for polling to SubmissionService.poll" in new Setup {
        worklist(submission)
        when(mockSubmissionService.poll(any())(any())).thenReturn(Future.successful(outcome(submission, polled = true)))

        service.invoke.futureValue mustBe Right(List("9200001"))

        val captor: ArgumentCaptor[SubmissionForPolling] = ArgumentCaptor.forClass(classOf[SubmissionForPolling])
        verify(mockSubmissionService).poll(captor.capture())(any())
        captor.getValue mustBe submission
      }

      "does not count a submission SubmissionService declined to poll" in new Setup {
        worklist(submission)
        when(mockSubmissionService.poll(any())(any())).thenReturn(Future.successful(outcome(submission, polled = false)))

        service.invoke.futureValue mustBe Right(Nil)
      }

      "polls every submission due, even when an earlier one was not polled" in new Setup {
        val second: SubmissionForPolling = submissionFor("9200002", "9600002")
        worklist(submission, second)
        when(mockSubmissionService.poll(eqTo(submission))(any())).thenReturn(Future.successful(outcome(submission, polled = false)))
        when(mockSubmissionService.poll(eqTo(second))(any())).thenReturn(Future.successful(outcome(second, polled = true)))

        service.invoke.futureValue mustBe Right(List("9200002"))
        verify(mockSubmissionService).poll(eqTo(second))(any())
      }
    }

    "when the job itself cannot run" - {

      "skips the run when the lock is held by another instance" in new Setup {
        when(mockLockRepository.takeLock(any(), any(), any())).thenReturn(Future.successful(None))

        service.invoke.futureValue mustBe Right(Nil)
      }

      "returns MongoUnlockException when the lock repository fails" in new Setup {
        val lockFailure = new RuntimeException("mongo down")
        when(mockLockRepository.takeLock(any(), any(), any())).thenReturn(Future.failed(lockFailure))

        service.invoke.futureValue mustBe Left(MongoUnlockException(lockFailure))
      }

      "returns FailedToPollSubmissions when fetching the submissions due for polling fails" in new Setup {
        val fetchFailure = new RuntimeException("formp-proxy unavailable")
        when(mockFormpConnector.getSubmissionsForPolling()(any())).thenReturn(Future.failed(fetchFailure))

        service.invoke.futureValue mustBe Left(FailedToPollSubmissions(fetchFailure))
      }
    }
  }
}
