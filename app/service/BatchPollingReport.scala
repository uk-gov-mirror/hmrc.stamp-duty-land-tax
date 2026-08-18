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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class PollOutcome(
  submission: SubmissionForPolling,
  polled: Boolean,
  pollResult: String,
  newReturnStatus: String,
  correlationId: String
)

object BatchPollingReport {

  private val DateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yy HH:mm:ss")

  private val SectionSeparator: String =
    "=========================================================================================================================================================================="

  private val ColumnHeadings: String =
    "    STORN          SUBMISSION_ID        RETURN_RESOURCE_REF      POLL_RESULT    NEW_RETURN_STATUS        CORRELATION ID"

  private val HeadingUnderline: String =
    "    ----------------------------------------------------------------------------------------------------------------------------------------------------------------------"

  def render(outcomes: List[PollOutcome], reportedAt: LocalDateTime): String =
    s"""
       |$SectionSeparator
       |BATCH POLLING RESULTS FOR ${reportedAt.format(DateFormatter)}
       |
       |$ColumnHeadings
       |$HeadingUnderline
       |${outcomes.map(row).mkString("\n")}
       |
       |$HeadingUnderline
       |
       |$SectionSeparator
       |""".stripMargin

  private def row(outcome: PollOutcome): String =
    "    " + List(
      column(outcome.submission.storn, 15),
      column(outcome.submission.submissionId, 20),
      column(outcome.submission.returnResourceRef, 24),
      column(outcome.pollResult, 15),
      column(outcome.newReturnStatus, 23),
      column(outcome.correlationId, 25)
    ).mkString(" ")

  private def column(value: String, width: Int): String =
    if (value.length > width) value.take(width - "...".length) + "..."
    else value.padTo(width, ' ')
}
