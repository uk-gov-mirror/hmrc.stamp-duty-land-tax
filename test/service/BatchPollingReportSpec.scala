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

import models.polling.SubmissionForPolling
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import java.time.LocalDateTime

class BatchPollingReportSpec extends AnyFreeSpec with Matchers {

  private val reportedAt = LocalDateTime.of(2026, 7, 24, 0, 30, 0)

  private def outcome(storn: String = "STN800",
                      submissionId: String = "9600001",
                      returnResourceRef: String = "9200001",
                      pollResult: String = "SUBMITTED",
                      newReturnStatus: String = "SUBMITTED",
                      correlationId: String = "CORRELATION123"): PollOutcome =
    PollOutcome(
      submission      = SubmissionForPolling(submissionId, storn, returnResourceRef, "ACCEPTED"),
      polled          = true,
      pollResult      = pollResult,
      newReturnStatus = newReturnStatus,
      correlationId   = correlationId
    )

  private def render(outcomes: PollOutcome*): String =
    BatchPollingReport.render(outcomes.toList, reportedAt).linesIterator.map(_.stripTrailing).mkString("\n")

  "BatchPollingReport" - {
    "render" - {

      "renders the banner with the run date and the column headings" in {
        val report = render(outcome())

        report must include("BATCH POLLING RESULTS FOR 24-07-26 00:30:00")
        report must include("    STORN          SUBMISSION_ID        RETURN_RESOURCE_REF      POLL_RESULT    NEW_RETURN_STATUS        CORRELATION ID")
      }

      "renders a submission across the F50 columns" in {
        render(outcome()) must include(
          "    STN800          9600001              9200001                  SUBMITTED       SUBMITTED               CORRELATION123"
        )
      }

      "renders one row per outcome, in the order they were selected" in {
        val report = render(
          outcome(submissionId = "9600001", returnResourceRef = "9200001"),
          outcome(submissionId = "9600002", returnResourceRef = "9200002")
        )

        report must include(
          """    STN800          9600001              9200001                  SUBMITTED       SUBMITTED               CORRELATION123
            |    STN800          9600002              9200002                  SUBMITTED       SUBMITTED               CORRELATION123""".stripMargin
        )
      }

      "truncates a value too long for its column and keeps the later columns in place" in {
        render(outcome(storn = "STORNAAAAABBBBBCCCCC")) must include(
          "    STORNAAAAABB... 9600001              9200001                  SUBMITTED       SUBMITTED               CORRELATION123"
        )
      }

      "renders the banner with no rows when nothing was selected" in {
        val report = render()

        report must include("BATCH POLLING RESULTS FOR 24-07-26 00:30:00")
        (report must not).include("STN800")
      }
    }
  }
}
