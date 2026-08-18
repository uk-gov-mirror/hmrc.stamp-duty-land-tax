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

package models.filing

import play.api.libs.json.*

enum ChrisDeleteResponse:
  case Deleted(correlationId: Option[String], raw: String)
  case NotFound(correlationId: Option[String], raw: String)
  case Errored(errors: Seq[GovTalkError], correlationId: Option[String], raw: String)
  case TransportError(message: String, raw: String = "")

enum SubmissionStatus(val value: String):
  case Pending  extends SubmissionStatus("PENDING")
  case Accepted extends SubmissionStatus("ACCEPTED")
  case Submitted extends SubmissionStatus("SUBMITTED")
  case SubmittedNoReceipt extends SubmissionStatus("SUBMITTED_NO_RECEIPT")
  case DepartmentalError  extends SubmissionStatus("DEPARTMENTAL_ERROR")
  case FatalError   extends SubmissionStatus("FATAL_ERROR")

object SubmissionStatus:
  def fromValue(s: String): Option[SubmissionStatus] =
    values.find(_.value.equalsIgnoreCase(s.trim))

  given Format[SubmissionStatus] = new Format[SubmissionStatus]:
    def reads(json: JsValue): JsResult[SubmissionStatus] = json match
      case JsString(s) =>
        SubmissionStatus.fromValue(s) match
          case Some(value) => JsSuccess(value)
          case None        => JsError(s"unknown SubmissionStatus: $s")
      case other => JsError(s"expected JsString for SubmissionStatus, got $other")
    def writes(s: SubmissionStatus): JsValue = JsString(s.value)


final case class GovTalkError(
                               raisedBy: String,
                               number: Option[String],
                               errorType: String,
                               text: Option[String],
                               location: Option[String]
                             ):
  def fromGateway: Boolean    = raisedBy.equalsIgnoreCase("gateway")
  def fromDepartment: Boolean = raisedBy.equalsIgnoreCase("department")

  def isBusiness: Boolean = errorType.equalsIgnoreCase("business") || number.contains("3001")

  def isFatal: Boolean = errorType.equalsIgnoreCase("fatal")

  def classification: String =
    if errorType.equalsIgnoreCase("recoverable") || number.contains("1100") then "recoverableError"
    else if isBusiness || fromDepartment then "departmentalError"
    else if errorType.equalsIgnoreCase("warning") then "warning"
    else if number.contains("1002") || number.contains("1046") then "gatewayUserError"
    else "systemError"

  def isDepartmental: Boolean = classification == "departmentalError"

object GovTalkError:
  given Format[GovTalkError] = Json.format[GovTalkError]

sealed trait ChrisResponse:
  def rawXml: String

  def toStatus: SubmissionStatus = this match
    case ChrisResponse.Completed(Some(_), _, _, _, _, _) => SubmissionStatus.Submitted
    case ChrisResponse.Completed(None, _, _, _, _, _)    => SubmissionStatus.FatalError
    case _: ChrisResponse.Acknowledged                => SubmissionStatus.Accepted
    case e: ChrisResponse.Errored                     =>
      if e.isBusinessReject then SubmissionStatus.DepartmentalError else SubmissionStatus.FatalError
    case _: ChrisResponse.TransportError              => SubmissionStatus.FatalError

object ChrisResponse:
  final case class Completed(
                              utrn: Option[String],
                              receivedIrMark: Option[String],
                              correlationId: Option[String],
                              responseEndPoint: Option[String],
                              rawXml: String,
                              acceptedTime: Option[String] = None
                            ) extends ChrisResponse

  final case class Errored(
                            errors: Seq[GovTalkError],
                            correlationId: Option[String],
                            responseEndPoint: Option[String],
                            rawXml: String
                          ) extends ChrisResponse:

    def isBusinessReject: Boolean = errors.exists(_.isDepartmental)

    def fieldErrors: Seq[GovTalkError] =
      val located = errors.filter(_.location.isDefined)
      if located.nonEmpty then located else errors

  final case class Acknowledged(
                                 correlationId: Option[String],
                                 pollIntervalSeconds: Option[Int],
                                 responseEndPoint: Option[String],
                                 rawXml: String,
                                 acceptedTime: Option[String] = None
                               ) extends ChrisResponse

  final case class TransportError(message: String, rawXml: String = "<transport-error/>") extends ChrisResponse


final case class SubmissionError(
                                  code: Option[String],
                                  message: String,
                                  location: Option[String] = None
                                )

object SubmissionError:
  given Format[SubmissionError] = Json.format[SubmissionError]

  def fromGovTalk(e: GovTalkError): SubmissionError =
    SubmissionError(
      code     = e.number,
      message  = e.text.getOrElse("Submission rejected"),
      location = e.location
    )


final case class SubmissionOutcome(
                                    returnId: String,
                                    status:   UniversalStatus,
                                    utrn:     Option[String],
                                    errors:   Seq[GovTalkError]
                                  )

object SubmissionOutcome:
  def fromChrisResponse(returnId: String, resp: ChrisResponse, expectedIrMark: Option[String]): SubmissionOutcome =
    SubmissionOutcome(
      returnId = returnId,
      status   = UniversalStatus.fromChrisResponse(resp, expectedIrMark),
      utrn     = resp match
        case c: ChrisResponse.Completed => c.utrn
        case _                          => None,
      errors   = resp match
        case e: ChrisResponse.Errored => e.errors
        case _                        => Nil
    )


sealed trait SubmissionResponse:
  def returnId: String

object SubmissionResponse:
  final case class Submitted(returnId: String, utrn: String, receipt: Boolean) extends SubmissionResponse
  final case class Acknowledged(returnId: String)                              extends SubmissionResponse
  final case class Retryable(returnId: String)                                 extends SubmissionResponse
  final case class Rejected(returnId: String, errors: Seq[SubmissionError])    extends SubmissionResponse
  final case class Failed(returnId: String, errors: Seq[SubmissionError])      extends SubmissionResponse

  private val AF11 = SubmissionError(None, "UC 1.44 AF11: the UTRN is not present in the submission response")

  def from(outcome: SubmissionOutcome): SubmissionResponse =
    val id = outcome.returnId
    outcome.status match
      case UniversalStatus.SUBMITTED =>
        outcome.utrn.fold[SubmissionResponse](Failed(id, Seq(AF11)))(u => Submitted(id, u, receipt = true))

      case UniversalStatus.SUBMITTED_NO_RECEIPT =>
        outcome.utrn.fold[SubmissionResponse](Failed(id, Seq(AF11)))(u => Submitted(id, u, receipt = false))

      case UniversalStatus.ACCEPTED =>
        Acknowledged(id)

      case UniversalStatus.STARTED =>
        Retryable(id)

      case UniversalStatus.DEPARTMENTAL_ERROR =>
        Rejected(id, outcome.errors.map(SubmissionError.fromGovTalk))

      case UniversalStatus.FATAL_ERROR =>
        Failed(id, outcome.errors.map(SubmissionError.fromGovTalk))

      case UniversalStatus.PENDING | UniversalStatus.VALIDATED =>
        Acknowledged(id)

  private val submittedFormat:    OFormat[Submitted]    = Json.format[Submitted]
  private val acknowledgedFormat: OFormat[Acknowledged] = Json.format[Acknowledged]
  private val retryableFormat:    OFormat[Retryable]    = Json.format[Retryable]
  private val rejectedFormat:     OFormat[Rejected]     = Json.format[Rejected]
  private val failedFormat:       OFormat[Failed]       = Json.format[Failed]

  given Format[SubmissionResponse] = new Format[SubmissionResponse]:
    def reads(json: JsValue): JsResult[SubmissionResponse] =
      (json \ "_type").asOpt[String] match
        case Some("submitted")    => submittedFormat.reads(json)
        case Some("acknowledged") => acknowledgedFormat.reads(json)
        case Some("retryable")    => retryableFormat.reads(json)
        case Some("rejected")     => rejectedFormat.reads(json)
        case Some("failed")       => failedFormat.reads(json)
        case Some(other)          => JsError(s"unknown SubmissionResponse _type: $other")
        case None                 => JsError("missing _type discriminator on SubmissionResponse")

    def writes(value: SubmissionResponse): JsValue = value match
      case s: Submitted    => submittedFormat.writes(s)    ++ Json.obj("_type" -> "submitted")
      case a: Acknowledged => acknowledgedFormat.writes(a) ++ Json.obj("_type" -> "acknowledged")
      case r: Retryable    => retryableFormat.writes(r)    ++ Json.obj("_type" -> "retryable")
      case r: Rejected     => rejectedFormat.writes(r)     ++ Json.obj("_type" -> "rejected")
      case f: Failed       => failedFormat.writes(f)       ++ Json.obj("_type" -> "failed")


final case class PersistedSubmission(
                                      returnId: String,
                                      storn: String,
                                      correlationId: String,
                                      status: SubmissionStatus,
                                      utrn: Option[String],
                                      govtalkErrors: Seq[GovTalkError]
                                    )

object PersistedSubmission:
  given Format[PersistedSubmission] = Json.format[PersistedSubmission]


enum UniversalStatus:
  case STARTED
  case VALIDATED
  case PENDING
  case ACCEPTED
  case SUBMITTED
  case SUBMITTED_NO_RECEIPT
  case DEPARTMENTAL_ERROR
  case FATAL_ERROR


object UniversalStatus:

  def fromString(in: String): Either[String, UniversalStatus] =
    in.toUpperCase() match
      case "STARTED"              => Right(UniversalStatus.STARTED)
      case "VALIDATED"            => Right(UniversalStatus.VALIDATED)
      case "ACCEPTED"             => Right(UniversalStatus.ACCEPTED)
      case "PENDING"              => Right(UniversalStatus.PENDING)
      case "SUBMITTED"            => Right(UniversalStatus.SUBMITTED)
      case "SUBMITTED_NO_RECEIPT" => Right(UniversalStatus.SUBMITTED_NO_RECEIPT)
      case "DEPARTMENTAL_ERROR"   => Right(UniversalStatus.DEPARTMENTAL_ERROR)
      case "FATAL_ERROR"          => Right(UniversalStatus.FATAL_ERROR)
      case status                 => Left(s"Unable to convert status: $status")

  private val ResetToStartedNumbers: Set[String] = Set("1000", "2005", "3000")

  def fromChrisResponse(resp: ChrisResponse, expectedIrMark: Option[String]): UniversalStatus =
    resp match
      case c: ChrisResponse.Completed =>
        if irMarkMatches(c.receivedIrMark, expectedIrMark) then UniversalStatus.SUBMITTED
        else UniversalStatus.SUBMITTED_NO_RECEIPT

      case _: ChrisResponse.Acknowledged =>
        UniversalStatus.ACCEPTED

      case e: ChrisResponse.Errored =>
        if isDepartmentalBusiness(e.errors) then UniversalStatus.DEPARTMENTAL_ERROR
        else if resetsToStarted(e.errors) then UniversalStatus.STARTED
        else UniversalStatus.FATAL_ERROR

      case t: ChrisResponse.TransportError =>
        if isTimeout(t.message) then UniversalStatus.STARTED
        else UniversalStatus.FATAL_ERROR

  private def irMarkMatches(received: Option[String], expected: Option[String]): Boolean =
    (received, expected) match
      case (Some(r), Some(e)) => r.trim.nonEmpty && r.trim.equalsIgnoreCase(e.trim)
      case _                  => false

  private def isDepartmentalBusiness(errors: Seq[GovTalkError]): Boolean =
    errors.exists(_.isDepartmental)

  def resetsToStarted(errors: Seq[GovTalkError]): Boolean =
    errors.exists(_.number.exists(ResetToStartedNumbers.contains))

  private def isTimeout(message: String): Boolean =
    val m = message.toLowerCase
    m.contains("timeout") || m.contains("timed out")



case class SubmitRequest(
                          email: Option[String] = None,
                          fullReturn: FullReturn
                        )

object SubmitRequest {
  implicit val reads: Reads[SubmitRequest] = Json.reads[SubmitRequest]
}