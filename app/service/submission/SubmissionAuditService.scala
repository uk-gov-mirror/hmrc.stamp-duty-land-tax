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

package service.submission

import com.google.inject.{Inject, Singleton}
import play.api.Logging
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import models.filing.*
import models.filing.FullReturn

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

final case class SubmissionAuditException(msg: String, cause: Option[Throwable] = None)
  extends RuntimeException(msg, cause.orNull)

@Singleton
class SubmissionAuditService @Inject() (
                                         auditConnector: AuditConnector,
                                         detailMapper: SdltAuditDetailMapper
                                       )(implicit ec: ExecutionContext)
  extends Logging:

  private val AuditSource  = "stamp-duty-land-tax"
  private val AuditSuccess = "SDLTSubmissionSuccess"
  private val AuditFailure = "SDLTSubmissionFailure"

  def auditSubmission(storn: String,
                      returnId: String,
                      correlationId: String,
                      fullReturn: FullReturn,
                      resp: ChrisResponse)
                     (implicit hc: HeaderCarrier): Future[Unit] =
    resp match
      case ChrisResponse.Completed(Some(utrn), _, _, _, _, _) =>
        send(AuditSuccess, successDetail(storn, utrn, fullReturn), returnId, correlationId)

      case ChrisResponse.Completed(None, _, _, _, _, _) =>
        logger.warn(s"[SubmissionAuditService] success envelope with no UTRN returnId=$returnId corrId=$correlationId")
        send(AuditFailure, failureDetail(storn, correlationId, "no_receipt", Nil, None), returnId, correlationId)

      case e: ChrisResponse.Errored =>
        val failureType = if e.isBusinessReject then "departmental" else "fatal"
        send(AuditFailure, failureDetail(storn, correlationId, failureType, e.errors, None), returnId, correlationId)

      case ChrisResponse.TransportError(msg, _) =>
        send(AuditFailure, failureDetail(storn, correlationId, "system", Nil, Some(msg)), returnId, correlationId)

      case _: ChrisResponse.Acknowledged =>
        logger.info(s"[SubmissionAuditService] acknowledged (in flight), no terminal audit returnId=$returnId corrId=$correlationId")
        Future.unit

  private def successDetail(storn: String, utrn: String, fullReturn: FullReturn): JsObject =
    Json.obj(
      "stampTaxesOnlineReferenceNumber"  -> storn,
      "uniqueTransactionReferenceNumber" -> utrn
    ) ++ detailMapper.submissionDetail(fullReturn)

  private def failureDetail(storn: String,
                            correlationId: String,
                            failureType: String,
                            errors: Seq[GovTalkError],
                            systemMessage: Option[String]): JsObject =
    Json.obj(
      "stampTaxesOnlineReferenceNumber" -> storn,
      "correlationId"                   -> correlationId,
      "failureType"                     -> failureType
    )
      ++ (if errors.nonEmpty then Json.obj("errors" -> Json.toJson(errors)) else Json.obj())
      ++ systemMessage.fold(Json.obj())(m => Json.obj("failureReason" -> m))

  private def send(auditType: String, detail: JsObject, returnId: String, correlationId: String)
                  (implicit hc: HeaderCarrier): Future[Unit] =
    val event = ExtendedDataEvent(
      auditProvider = Some("mdtp"),
      auditSource   = AuditSource,
      auditType     = auditType,
      detail        = detail
    )

    auditConnector.sendExtendedEvent(event).flatMap {
      case AuditResult.Success =>
        logger.debug(s"[SubmissionAuditService] audit OK type=$auditType returnId=$returnId corrId=$correlationId")
        Future.unit

      case AuditResult.Disabled =>
        logger.info(s"[SubmissionAuditService] audit disabled, skipping type=$auditType returnId=$returnId corrId=$correlationId")
        Future.unit

      case AuditResult.Failure(msg, throwableOpt) =>
        val e = throwableOpt.orNull
        logger.error(s"[SubmissionAuditService] audit FAILED type=$auditType returnId=$returnId corrId=$correlationId: $msg", e)
        Future.failed(SubmissionAuditException(s"Audit failed: $msg", Option(e)))
    }.recoverWith {
      case e: SubmissionAuditException =>
        Future.failed(e)
      case NonFatal(e) =>
        logger.error(s"[SubmissionAuditService] audit exception type=$auditType returnId=$returnId corrId=$correlationId", e)
        Future.failed(SubmissionAuditException("Audit threw an exception", Some(e)))
}