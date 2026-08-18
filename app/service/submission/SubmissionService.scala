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

package service.submission

import com.google.inject.Inject
import play.api.Logging
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}
import connectors.{ChrisConnector, EmailServiceConnector}
import connectors.FilingFormpProxyConnector
import models.email.EmailServiceRequest
import models.filing.*
import models.polling.SubmissionForPolling
import models.submission.*
import service.PollOutcome
import service.filing.ChrisService

import java.time.{LocalDate, ZoneOffset, ZonedDateTime}
import java.time.{Clock, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

final case class SchemaValidationException(validationErrors: Seq[String])
  extends RuntimeException(s"SDLT payload failed schema validation:\n${validationErrors.mkString("\n")}")

final case class MissingSubmissionContextException(message: String)
  extends RuntimeException(message)

final case class ReturnLockConflictException(returnId: String, status: Int, message: String)
  extends RuntimeException(s"Could not lock return $returnId (HTTP $status): $message")

final case class GovTalkLockNotAcquiredException(formResultId: String, cause: Throwable)
  extends RuntimeException(s"Could not acquire GovTalk Status lock for formResultId=$formResultId", cause)

final case class MissingSubmissionIdException(returnId: String)
  extends RuntimeException(s"No submissionId available for return $returnId; cannot key GovTalk status by Submission ID")

class SubmissionService @Inject() (
                                    envelopeBuilder: GovTalkEnvelopeBuilder,
                                    validator: SchemaValidator,
                                    connector: ChrisConnector,
                                    audit: SubmissionAuditService,
                                    chrisService: ChrisService,
                                    emailService: EmailService,
                                    filingConnector: FilingFormpProxyConnector,
                                    clock: Clock
                                  )(implicit ec: ExecutionContext)
  extends Logging:

  def submit(fullReturn: FullReturn,
             sender: SenderType,
             periodEnd: LocalDate,
             credentialIdentifier: String,
             email: Option[String] = None)(implicit hc: HeaderCarrier): Future[SubmissionOutcome] =
    val correlationId = newCorrelationId()

    requireContext(fullReturn, credentialIdentifier) match
      case Left(msg) =>
        logger.error(s"[SubmissionService] $msg")
        Future.failed(MissingSubmissionContextException(msg))

      case Right(ctx0) =>
        logger.info(s"[SubmissionService] submit START returnId=${ctx0.returnId} storn=${ctx0.storn} version=${ctx0.version} sender=$sender periodEnd=$periodEnd hasExistingSubmission=${fullReturn.submission.isDefined} corrId=$correlationId")
        val started: Future[SubmissionOutcome] = for
          submissionId     <- prepareReturn(ctx0, fullReturn, correlationId, email)
          ctx              =  ctx0.copy(submissionId = submissionId)
          submitUrl        <- prepareGovTalkStatus(ctx, correlationId)
          built            <- buildAndValidate(ctx, fullReturn, sender, periodEnd, correlationId)
          (envelope, seed) = built
          sentIrMark       = seed.IRMarkSent.getOrElse("")
          _                <- acquireGovTalkLock(ctx, correlationId)
          resp             <- sendAndHandle(ctx, fullReturn, envelope, seed, sentIrMark, submitUrl, correlationId, email)
        yield SubmissionOutcome.fromChrisResponse(ctx0.returnId, resp, Some(sentIrMark))

        started.andThen {
          case Success(outcome) =>
            logger.info(s"[SubmissionService] submit END returnId=${ctx0.returnId} corrId=$correlationId status=${outcome.status} utrn=${outcome.utrn.getOrElse("-")}")
          case Failure(e) =>
            logger.error(s"[SubmissionService] submit FAILED returnId=${ctx0.returnId} corrId=$correlationId error=${e.getClass.getSimpleName}: ${e.getMessage}")
        }

  private def prepareReturn(ctx: SubmissionContext, fullReturn: FullReturn, correlationId: String, email: Option[String] = None)
                           (implicit hc: HeaderCarrier): Future[String] =
    for
      _            <- lockReturn(ctx, correlationId)
      submissionId <- handleExistingSubmission(ctx, fullReturn, correlationId, email)
    yield submissionId

  private def lockReturn(ctx: SubmissionContext, correlationId: String)(implicit hc: HeaderCarrier): Future[Unit] =
    logger.info(s"[SubmissionService] locking return returnId=${ctx.returnId} version=${ctx.version} corrId=$correlationId")
    chrisService.lockReturn(LockReturnRequest(ctx.storn, ctx.returnId, ctx.version)).flatMap {
      case Right(_) =>
        logger.info(s"[SubmissionService] return lock ACQUIRED returnId=${ctx.returnId} corrId=$correlationId")
        Future.successful(())
      case Left(error) =>
        logger.warn(s"[SubmissionService] return lock CONFLICT returnId=${ctx.returnId} corrId=$correlationId: ${error.statusCode} ${error.message}")
        Future.failed(ReturnLockConflictException(ctx.returnId, error.statusCode, error.message))
    }
  
  private def handleExistingSubmission(ctx: SubmissionContext, fullReturn: FullReturn, correlationId: String, email: Option[String] = None)
                                      (implicit hc: HeaderCarrier): Future[String] =
    fullReturn.submission match
      case Some(existing) if isResubmittable(existing) =>
        requireSubmissionId(existing.submissionID, ctx) { submissionId =>
          logger.info(s"[SubmissionService] existing submission is re-submittable (status=${existing.submissionStatus.getOrElse("-")}, submissionId=$submissionId); clearing prior error details returnId=${ctx.returnId} corrId=$correlationId")
          chrisService.deleteSubmissionErrorDetail(DeleteSubmissionErrorDetailRequest(ctx.storn, ctx.returnId)).map(_ => submissionId)
        }

      case Some(existing) =>
        requireSubmissionId(existing.submissionID, ctx) { submissionId =>
          logger.info(s"[SubmissionService] existing submission present (status=${existing.submissionStatus.getOrElse("-")}, submissionId=$submissionId) returnId=${ctx.returnId} corrId=$correlationId")
          Future.successful(submissionId)
        }

      case None =>
        logger.info(s"[SubmissionService] no existing submission; creating new submission returnId=${ctx.returnId} corrId=$correlationId")
        chrisService.createSubmission(CreateSubmissionRequest(ctx.storn, ctx.returnId, email)).flatMap { ret =>
          requireSubmissionId(ret.submissionId, ctx) { submissionId =>
            logger.info(s"[SubmissionService] new submission created submissionId=$submissionId returnId=${ctx.returnId} corrId=$correlationId")
            Future.successful(submissionId)
          }
        }
  
  private def requireSubmissionId[A](id: Option[String], ctx: SubmissionContext)(f: String => Future[A]): Future[A] =
    id.map(_.trim).filter(_.nonEmpty) match
      case Some(submissionId) => f(submissionId)
      case None =>
        logger.error(s"[SubmissionService] missing submissionId returnId=${ctx.returnId}; cannot key GovTalk status")
        Future.failed(MissingSubmissionIdException(ctx.returnId))

  private def isResubmittable(existing: Submission): Boolean =
    existing.submissionStatus.exists { s =>
      val v = s.trim.toUpperCase
      v == "DEPARTMENTAL_ERROR" || v == "FATAL_ERROR" || v == "STARTED"
    }


  private def prepareGovTalkStatus(ctx: SubmissionContext, correlationId: String)
                                  (implicit hc: HeaderCarrier): Future[Option[String]] =
    selectGovTalkStatus(ctx).flatMap {
      case Some(existing) =>
        logger.info(s"[SubmissionService] GovTalk Status row FOUND formResultId=${ctx.formResultId} corrId=$correlationId protocolStatus=${existing.protocolStatus.getOrElse("-")} storedGatewayUrl=${existing.gatewayUrl.getOrElse("-")} row=$existing")
        existing.protocolStatus match
          case Some(status) if status.nonEmpty =>
            val storedUrl = existing.gatewayUrl.map(_.trim).filter(_.nonEmpty)
            logger.info(s"[SubmissionService] RESETTING GovTalk Status formResultId=${ctx.formResultId} corrId=$correlationId oldProtocol=$status storedGatewayUrl=${storedUrl.getOrElse("-")} -> this value will be used as the ChRIS submit URL")
            chrisService
              .resetGovTalkStatus(buildResetRequest(ctx.storn, ctx.formResultId, status))
              .flatMap { _ =>
                logger.info(s"[SubmissionService] GovTalk Status reset OK formResultId=${ctx.formResultId} corrId=$correlationId resolvedSubmitUrl=${storedUrl.getOrElse("<default>")}")
                setGovTalkCorrelationId(ctx.storn, ctx.formResultId, correlationId)
                  .recoverWith { case e =>
                    logger.error(s"[SubmissionService] aborting submit reason=correlation-id-write-failed returnId=${ctx.returnId} corrId=$correlationId: ${e.getMessage}")
                    Future.failed(e)
                  }
                  .map(_ => None)
              }

          case _ =>
            logger.warn(s"[SubmissionService] GovTalk Status row present but protocolStatus EMPTY formResultId=${ctx.formResultId} corrId=$correlationId; deleting and re-inserting")
            deleteThenInsert(ctx, correlationId).map { _ =>
              logger.info(s"[SubmissionService] GovTalk Status delete+insert OK formResultId=${ctx.formResultId} corrId=$correlationId resolvedSubmitUrl=<default>")
              None
            }
      case None =>
        logger.info(s"[SubmissionService] no GovTalk Status row; inserting initial row formResultId=${ctx.formResultId} corrId=$correlationId")
        chrisService.insertInitialGovTalkStatus(buildInitialInsertRequest(ctx, correlationId)).map { _ =>
          logger.info(s"[SubmissionService] initial GovTalk Status inserted formResultId=${ctx.formResultId} corrId=$correlationId resolvedSubmitUrl=<default>")
          None
        }
    }

  private def deleteThenInsert(ctx: SubmissionContext, correlationId: String)
                              (implicit hc: HeaderCarrier): Future[Unit] =
    logger.info(s"[SubmissionService] deleting GovTalk Status row formResultId=${ctx.formResultId} corrId=$correlationId")
    chrisService.deleteGovTalkStatus(DeleteGovTalkStatusRequest(resultId = ctx.formResultId))
      .flatMap { _ =>
        logger.info(s"[SubmissionService] GovTalk Status row deleted; re-inserting initial row formResultId=${ctx.formResultId} corrId=$correlationId")
        chrisService.insertInitialGovTalkStatus(buildInitialInsertRequest(ctx, correlationId))
          .map(_ => ())
          .recoverWith { case e =>
            logger.error(
              s"[SubmissionService] delete succeeded but insert FAILED — GovTalk Status row for " +
                s"formResultId=${ctx.formResultId} corrId=$correlationId is now MISSING, must re-seed on retry", e)
            Future.failed(e)
          }
      }

  private def selectGovTalkStatus(ctx: SubmissionContext)(implicit hc: HeaderCarrier): Future[Option[SelectGovTalkStatusResponse]] =
    logger.info(s"[SubmissionService] selecting GovTalk Status formResultId=${ctx.formResultId} storn=${ctx.storn} corrId=?")
    chrisService.selectGovTalkStatus(SelectGovTalkStatusRequest(ctx.storn, ctx.formResultId))
      .map(resp => Option.when(resp.formResultId.exists(_.trim.nonEmpty))(resp))
      .recover { case e =>
        logger.warn(s"[SubmissionService] selectGovTalkStatus lookup failed (treating as no row) formResultId=${ctx.formResultId}: ${e.getMessage}")
        None
      }

  private def buildInitialInsertRequest(ctx: SubmissionContext, correlationId: String): InsertInitialGovTalkStatusRequest =
    val now = nowSqlTimestamp
    InsertInitialGovTalkStatusRequest(
      userIdentifier = ctx.storn,
      formResultId   = ctx.formResultId,
      correlationId  = correlationId,
      govTalkStatus  = GovTalkStatusInitial(
        formLock             = "N",
        createTimestamp      = now,
        endStateTimestamp    = None,
        lastMessageTimestamp = now,
        numberOfPolls        = "0",
        pollInterval         = "0",
        protocolStatus       = "initial",
        gatewayUrl           = connector.defaultPath
      )
    )

  private def buildResetRequest(storn: String, formResultId: String, oldProtocol: String): ResetGovTalkStatusRequest =
    val now = nowSqlTimestamp
    ResetGovTalkStatusRequest(
      userIdentifier = storn,
      formResultId   = formResultId,
      correlationId  = "empty",
      govTalkStatus  = GovTalkStatusReset(
        formLock             = "N",
        createTimestamp      = now,
        endStateTimestamp    = None,          // spec F53 step 7: null for the end state date on reset
        lastMessageTimestamp = now,
        numberOfPolls        = "0",
        pollInterval         = "0",
        protocolStatusOld    = oldProtocol,
        protocolStatusNew    = "initial",
        gatewayUrl           = connector.defaultPath
      )
    )

  private def buildAndValidate(ctx: SubmissionContext,
                               fullReturn: FullReturn,
                               sender: SenderType,
                               periodEnd: LocalDate,
                               correlationId: String)
                              (implicit hc: HeaderCarrier): Future[(Elem, SubmissionUpdate)] =
    logger.info(s"[SubmissionService] building and validating SDLT payload returnId=${ctx.returnId} corrId=$correlationId")
    val sdlt = SdltReturnMapper.toSdltElement(fullReturn)

    validator.validateSdlt(sdlt) match
      case Left(errors) =>
        logger.error(s"[SubmissionService] schema validation FAILED returnId=${ctx.returnId} corrId=$correlationId: ${errors.mkString("; ")}")
        Future.failed(SchemaValidationException(errors))

      case Right(_) =>
        logger.info(s"[SubmissionService] schema validation OK returnId=${ctx.returnId} corrId=$correlationId")
        val irMarkResult = envelopeBuilder.submissionRequest(sdlt, ctx.storn, periodEnd, sender, ctx.credentialIdentifier, correlationId)
        logger.info(s"[SubmissionService] IRmark computed returnId=${ctx.returnId} corrId=$correlationId b32=${irMarkResult.base32}")

        val pending = SubmissionUpdate(
          IRMarkRecieved        = None,
          utrn                  = None,
          email                 = None,
          submissionRequestDate = fullReturn.submission.flatMap(_.submissionRequestDate),
          acceptedDate          = None,
          submittableStatus     = Some("PENDING"),
          govTalkErrorCode      = None,
          govTalkErrorType      = None,
          govTalkErrorMessage   = None,
          IRMarkSent            = Some(irMarkResult.base64)
        )

        chrisService
          .updateSubmission(UpdateSubmissionRequest(ctx.storn, ctx.returnId, pending))
          .map { _ =>
            logger.info(s"[SubmissionService] PENDING seed persisted returnId=${ctx.returnId} corrId=$correlationId IRMarkSent=${irMarkResult.base64}")
            (irMarkResult.envelope, pending)
          }

  private def acquireGovTalkLock(ctx: SubmissionContext, correlationId: String)(implicit hc: HeaderCarrier): Future[Unit] =
    logger.info(s"[SubmissionService] acquiring GovTalk lock (N->Y) formResultId=${ctx.formResultId} corrId=$correlationId")
    val req = UpdateGovTalkStatusLockRequest(
      userIdentifier = ctx.storn,
      formResultId   = ctx.formResultId,
      govTalkStatus  = GovTalkStatusLock(formLockOld = "N", formLockNew = "Y", pollInterval = "0", gatewayUrl = connector.defaultPath)
    )
    chrisService.updateGovTalkStatusLock(req).map { _ =>
      logger.info(s"[SubmissionService] GovTalk lock ACQUIRED formResultId=${ctx.formResultId} corrId=$correlationId")
      ()
    }.recoverWith { case e =>
      logger.error(s"[SubmissionService] GovTalk lock NOT acquired formResultId=${ctx.formResultId} corrId=$correlationId", e)
      Future.failed(GovTalkLockNotAcquiredException(ctx.formResultId, e))
    }

  private def selectGovTalkRow(storn: String, formResultId: String)
                               (implicit hc: HeaderCarrier): Future[Option[SelectGovTalkStatusResponse]] =
    chrisService.selectGovTalkStatus(SelectGovTalkStatusRequest(storn, formResultId))
      .map(row => Option.when(row.formResultId.exists(_.trim.nonEmpty))(row))

  private def releaseGovTalkLock(storn: String, formResultId: String, ref: String, correlationId: String, fallback: Option[SelectGovTalkStatusResponse] = None)
                                (implicit hc: HeaderCarrier): Future[Unit] =
    selectGovTalkRow(storn, formResultId)
      .recover { case e =>
        val consequence =
          if fallback.isDefined then "fallback=pre-poll-row"
          else "fallback=defaults"
        logger.warn(s"[$logPrefix] lock release re-read failed, $consequence ${logRef(storn, ref, correlationId)}: ${e.getMessage}")
        None
      }
      .flatMap {
        case Some(fresh) if !fresh.formLock.map(_.trim).contains("Y") =>
          logger.info(s"[$logPrefix] GovTalk lock already released ${logRef(storn, ref, correlationId)}")
          Future.unit
        case fresh =>
          val current = fresh.orElse(fallback)
          chrisService.updateGovTalkStatusLock(buildLockRequest(
            storn,
            formResultId,
            formLockOld  = "Y",
            formLockNew  = "N",
            pollInterval = current.map(pollIntervalOf).getOrElse("0"),
            gatewayUrl   = current.flatMap(_.gatewayUrl)
          )).map { _ =>
            logger.info(s"[$logPrefix] GovTalk lock released ${logRef(storn, ref, correlationId)}")
            ()
          }
      }
      .recover { case e =>
        logger.error(s"[$logPrefix] lock release failed, row stays locked ${logRef(storn, ref, correlationId)}: ${e.getMessage}")
        ()
      }

  private def sendAndHandle(ctx: SubmissionContext,
                            fullReturn: FullReturn,
                            envelope: Elem,
                            seed: SubmissionUpdate,
                            sentIrMark: String,
                            submitUrl: Option[String],
                            correlationId: String,
                            email: Option[String] = None)
                           (implicit hc: HeaderCarrier): Future[ChrisResponse] =
    logger.info(s"[SubmissionService] SUBMITTING to ChRIS returnId=${ctx.returnId} corrId=$correlationId submitUrl=${submitUrl.getOrElse("<default (connector fallback)>")}")

    val work: Future[(ChrisResponse, SubmissionUpdate)] =
      for
        resp     <- connector.submit(envelope, submitUrl, correlationId, Some(ctx.returnId))
        _        =  logger.info(s"[SubmissionService] ChRIS response received returnId=${ctx.returnId} corrId=$correlationId response=${resp.getClass.getSimpleName}")
        finalAcc <- handleResponse(ctx, fullReturn, resp, seed, sentIrMark, correlationId, email)
      yield (resp, finalAcc)

    work.transformWith { outcome =>
      val skipDatetime = outcome match
        case Success((resp, _)) => isRecoverableResp(resp)
        case Failure(_)         => true

      logger.info(s"[SubmissionService] post-submit cleanup returnId=${ctx.returnId} corrId=$correlationId skipDatetimeFooter=$skipDatetime outcome=${outcome.fold(_.getClass.getSimpleName, _._1.getClass.getSimpleName)}")

      val stampDatetime: Future[Unit] = outcome match
        case Success((_, finalAcc)) if !skipDatetime && !hasSubmissionRequestDate(fullReturn) =>
          ensureSubmissionRequestDatetime(ctx, finalAcc, correlationId)
        case _ =>
          Future.unit

      (for
        _ <- releaseGovTalkLock(ctx.storn, ctx.formResultId, ctx.returnId, correlationId)
        _ <- stampDatetime
      yield ()).transformWith(_ => Future.fromTry(outcome.map(_._1)))
    }

  private val RecoverableNumbers: Set[String] = Set("1000", "2005", "3000")


  private def isRecoverable(errors: Seq[GovTalkError]): Boolean =
    errors.exists(_.number.exists(RecoverableNumbers.contains))

  private def isRecoverableResp(resp: ChrisResponse): Boolean = resp match
    case e: ChrisResponse.Errored => isRecoverable(e.errors)
    case _ => false

  private def handleResponse(ctx: SubmissionContext,
                             fullReturn: FullReturn,
                             resp: ChrisResponse,
                             seed: SubmissionUpdate,
                             sentIrMark: String,
                             correlationId: String,
                             email: Option[String] = None)
                            (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    val universal = UniversalStatus.fromChrisResponse(resp, Some(sentIrMark))
    logger.info(s"[SubmissionService] resolved returnId=${ctx.returnId} corrId=$correlationId universalStatus=$universal responseType=${resp.getClass.getSimpleName}")

    adoptChrisCorrelationId(ctx, resp, correlationId).flatMap { (corrId, stored) =>
    resp match
      case c: ChrisResponse.Completed =>
        logger.info(s"[SubmissionService] handling COMPLETED returnId=${ctx.returnId} corrId=$correlationId utrn=${c.utrn.getOrElse("-")} receivedIrMark=${c.receivedIrMark.getOrElse("-")}")
        successBranch(ctx, fullReturn, c, universal, seed, corrId, email)

      case a: ChrisResponse.Acknowledged =>
        logger.info(s"[SubmissionService] handling ACKNOWLEDGED returnId=${ctx.returnId} corrId=$correlationId pollInterval=${a.pollIntervalSeconds.getOrElse(0)}")
        acknowledgementBranch(ctx, a, seed, corrId, stored)

      case e: ChrisResponse.Errored =>
        logger.warn(s"[SubmissionService] handling ERRORED returnId=${ctx.returnId} corrId=$correlationId errorCount=${e.errors.size} numbers=${e.errors.flatMap(_.number).mkString(",")}")
        errorBranch(ctx, fullReturn, e.errors, e.responseEndPoint, universal, seed, corrId)

      case ChrisResponse.TransportError(msg, _) =>
        logger.error(s"[SubmissionService] handling TRANSPORT ERROR returnId=${ctx.returnId} corrId=$correlationId: $msg")
        errorBranch(ctx, fullReturn, Nil, None, universal, seed, corrId)
    }

  private def adoptChrisCorrelationId(ctx: SubmissionContext, resp: ChrisResponse, sent: String)
                                     (implicit hc: HeaderCarrier): Future[(String, Boolean)] =
    val assigned = resp match
      case c: ChrisResponse.Completed    => c.correlationId
      case a: ChrisResponse.Acknowledged => a.correlationId
      case e: ChrisResponse.Errored      => e.correlationId
      case _: ChrisResponse.TransportError => None

    assigned.map(_.trim).filter(id => id.nonEmpty && id != sent) match
      case Some(fromChris) =>
        logger.info(s"[SubmissionService] adopting ChRIS correlation id returnId=${ctx.returnId} sent=$sent assigned=$fromChris")
        setGovTalkCorrelationId(ctx.storn, ctx.formResultId, fromChris)
          .map(_ => (fromChris, true))
          .recover { case e =>
            logger.error(s"[SubmissionService] assigned correlation id not stored returnId=${ctx.returnId} assigned=$fromChris: ${e.getMessage}")
            (fromChris, false)
          }
      case None =>
        Future.successful((sent, true))

  private def setGovTalkCorrelationId(storn: String, formResultId: String, correlationId: String)
                                     (implicit hc: HeaderCarrier): Future[Unit] =
    chrisService.updateGovTalkStatusCorrelationId(UpdateGovTalkStatusCorrelationIdRequest(
      userIdentifier = storn,
      formResultId   = formResultId,
      correlationId  = correlationId,
      pollInterval   = 0,
      gatewayUrl     = connector.defaultPath
    )).map { _ =>
      logger.info(s"[$logPrefix] GovTalk correlation id stored ${logRef(storn, formResultId, correlationId)}")
      ()
    }

  private def successBranch(ctx: SubmissionContext,
                            fullReturn: FullReturn,
                            resp: ChrisResponse.Completed,
                            universal: UniversalStatus,
                            seed: SubmissionUpdate,
                            correlationId: String,
                            email: Option[String] = None)
                           (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    logger.info(s"[SubmissionService] SUCCESS branch returnId=${ctx.returnId} corrId=$correlationId universalStatus=$universal responseEndPoint=${resp.responseEndPoint.getOrElse("-")}")
    for
      acc1    <- persistStatus(ctx, seed, universal, correlationId)
      _       <- updateGovTalkStatistics(ctx.storn, ctx.formResultId, resp.responseEndPoint, None, correlationId)
      _       <- setGovTalkProtocol(ctx.storn, ctx.formResultId, "deleteRequest", correlationId)
      deleted <- sendChrisDelete(ctx.storn, ctx.returnId, resp.responseEndPoint, correlationId)
      _       <- if deleted then finaliseGovTalkStatus(ctx.storn, ctx.formResultId, correlationId)
      else {
        logger.warn(s"[SubmissionService] ChRIS delete unsuccessful; leaving GovTalk at deleteRequest (no endState/reset) returnId=${ctx.returnId} corrId=$correlationId")
        Future.unit
      }
      _       <- audit.auditSubmission(ctx.storn, ctx.returnId, correlationId, fullReturn, resp)
      acc2    <- persistUpdate(ctx.storn, ctx.returnId, acc1.copy(
        IRMarkRecieved = resp.receivedIrMark,
        utrn           = resp.utrn,
        acceptedDate   = Some(resp.acceptedTime.getOrElse(nowIso))
      ), correlationId)
      _ <- emailService.submitEmailConfirmation(fullReturn, resp.utrn.toString, email)
      _       =  logger.info(s"[SubmissionService] SUCCESS branch complete returnId=${ctx.returnId} corrId=$correlationId utrn=${resp.utrn.getOrElse("-")}")
    yield acc2

  // spec F52 step 14.1.3: after a successful delete, mark endState and then RESET the GovTalk status row.
  // Error-suppressed: this is post-success housekeeping and must never turn an accepted filing into a failure.
  private def finaliseGovTalkStatus(storn: String, formResultId: String, correlationId: String)(implicit hc: HeaderCarrier): Future[Unit] =
    (for {
      _ <- setGovTalkProtocol(storn, formResultId, "endState", correlationId)
      _ <- chrisService.resetGovTalkStatus(buildResetRequest(storn, formResultId, "endState")).map(_ => ())
    } yield {
      logger.info(s"[SubmissionService] GovTalk Status finalised (endState + reset) formResultId=$formResultId corrId=$correlationId")
      ()
    }).recover { case e =>
      logger.warn(s"[SubmissionService] GovTalk finalise (endState/reset) FAILED (suppressed) formResultId=$formResultId corrId=$correlationId: ${e.getMessage}")
      ()
    }

  private def acknowledgementBranch(ctx: SubmissionContext,
                                    resp: ChrisResponse.Acknowledged,
                                    seed: SubmissionUpdate,
                                    correlationId: String,
                                    correlationIdStored: Boolean)
                                   (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    logger.info(s"[SubmissionService] ACK branch returnId=${ctx.returnId} corrId=$correlationId pollInterval=${resp.pollIntervalSeconds.getOrElse(0)} responseEndPoint=${resp.responseEndPoint.getOrElse("-")}")
    for
      acc1 <- persistStatus(ctx, seed, UniversalStatus.ACCEPTED, correlationId)
      _    <- if correlationIdStored then setGovTalkProtocol(ctx.storn, ctx.formResultId, "dataPoll", correlationId)
              else
                logger.error(s"[SubmissionService] GovTalk row unpollable reason=correlation-id-write-failed returnId=${ctx.returnId} corrId=$correlationId")
                Future.unit
      acc2 <- persistUpdate(ctx.storn, ctx.returnId, acc1.copy(acceptedDate = Some(resp.acceptedTime.getOrElse(nowIso))), correlationId)
      _    <- updateGovTalkStatistics(ctx.storn, ctx.formResultId, resp.responseEndPoint, resp.pollIntervalSeconds, correlationId)
      _    =  logger.info(s"[SubmissionService] ACK branch complete returnId=${ctx.returnId} corrId=$correlationId")
    yield acc2

  private def errorBranch(ctx: SubmissionContext,
                          fullReturn: FullReturn,
                          errors: Seq[GovTalkError],
                          responseEndPoint: Option[String],
                          universal: UniversalStatus,
                          seed: SubmissionUpdate,
                          correlationId: String)
                         (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    val deptError = universal == UniversalStatus.DEPARTMENTAL_ERROR
    val first     = errors.headOption

    logger.warn(s"[SubmissionService] ERROR branch returnId=${ctx.returnId} corrId=$correlationId universalStatus=$universal departmental=$deptError firstErrorNumber=${first.flatMap(_.number).getOrElse("-")} firstErrorType=${first.map(_.errorType).getOrElse("-")} firstErrorText=${first.flatMap(_.text).getOrElse("-")}")

    val govTalkForDept: Future[Unit] =
      if deptError then
        logger.info(s"[SubmissionService] departmental error — driving GovTalk delete/endState returnId=${ctx.returnId} corrId=$correlationId")
        for
          _ <- updateGovTalkStatistics(ctx.storn, ctx.formResultId, responseEndPoint, None, correlationId)
          _ <- setGovTalkProtocol(ctx.storn, ctx.formResultId, "deleteRequest", correlationId)
          _ <- sendChrisDelete(ctx.storn, ctx.returnId, responseEndPoint, correlationId)
          _ <- setGovTalkProtocol(ctx.storn, ctx.formResultId, "endState", correlationId)
        yield ()
      else Future.unit

    for
      acc1 <- persistStatus(ctx, seed, universal, correlationId)
      _    <- govTalkForDept
      acc2 <- persistUpdate(ctx.storn, ctx.returnId, acc1.copy(
        govTalkErrorCode    = first.flatMap(_.number),
        govTalkErrorType    = first.map(_.classification),
        govTalkErrorMessage = first.flatMap(_.text)
      ), correlationId)
      _    <- createSubmissionErrorDetails(ctx.storn, ctx.returnId, errors, correlationId)
      acc3 <- recoverableTail(ctx.storn, ctx.returnId, acc2, errors, correlationId).map(_.getOrElse(acc2))
      _    <- audit.auditSubmission(ctx.storn, ctx.returnId, correlationId, fullReturn,
        ChrisResponse.Errored(errors, Some(correlationId), responseEndPoint, "<error/>")) // CIP failure
      _    =  logger.info(s"[SubmissionService] ERROR branch complete returnId=${ctx.returnId} corrId=$correlationId recoverable=${isRecoverable(errors)}")
    yield acc3

  private def recoverableTail(storn: String, returnId: String, acc: SubmissionUpdate, errors: Seq[GovTalkError], correlationId: String)
                                (implicit hc: HeaderCarrier): Future[Option[SubmissionUpdate]] =
    if isRecoverable(errors) then
      logger.info(s"[SubmissionService] error IS recoverable — overwriting status to STARTED and clearing request datetime returnId=$returnId corrId=$correlationId")
      persistUpdate(storn, returnId, acc.copy(
        submittableStatus     = Some(UniversalStatus.STARTED.toString),
        submissionRequestDate = None
      ), correlationId).map(Some(_))
    else
      logger.info(s"[SubmissionService] error NOT recoverable returnId=$returnId corrId=$correlationId")
      Future.successful(None)

  private def persistStatus(ctx: SubmissionContext, acc: SubmissionUpdate, status: UniversalStatus, correlationId: String)
                           (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    persistUpdate(ctx.storn, ctx.returnId, acc.copy(submittableStatus = Some(status.toString)), correlationId)

  private def persistUpdate(storn: String, returnId: String, acc: SubmissionUpdate, correlationId: String)
                           (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    chrisService.updateSubmission(UpdateSubmissionRequest(storn, returnId, acc)).map { _ =>
      logger.info(s"[SubmissionService] submission updated status=${acc.submittableStatus.getOrElse("-")} utrn=${acc.utrn.getOrElse("-")} returnId=$returnId corrId=$correlationId")
      acc
    }

  private def hasSubmissionRequestDate(fullReturn: FullReturn): Boolean =
    fullReturn.submission.flatMap(_.submissionRequestDate).exists(_.trim.nonEmpty)

  private def ensureSubmissionRequestDatetime(ctx: SubmissionContext, base: SubmissionUpdate, correlationId: String)
                                             (implicit hc: HeaderCarrier): Future[Unit] =
    val update = base.copy(submissionRequestDate = Some(nowIso))
    chrisService.updateSubmission(UpdateSubmissionRequest(ctx.storn, ctx.returnId, update)).map { _ =>
      logger.info(s"[SubmissionService] submission-request datetime ensured returnId=${ctx.returnId} corrId=$correlationId")
      ()
    }.recover { case e =>
      logger.warn(s"[SubmissionService] ensure submission-request datetime FAILED (suppressed) returnId=${ctx.returnId} corrId=$correlationId: ${e.getMessage}")
      ()
    }

  private def createSubmissionErrorDetails(storn: String, returnId: String, errors: Seq[GovTalkError], correlationId: String)
                                          (implicit hc: HeaderCarrier): Future[Unit] =
    if errors.isEmpty then
      logger.info(s"[SubmissionService] no GovTalk errors to persist returnId=$returnId corrId=$correlationId")
      Future.unit
    else
      logger.info(s"[SubmissionService] persisting ${errors.size} GovTalk error detail(s) returnId=$returnId corrId=$correlationId")
      errors.zipWithIndex.foldLeft(Future.unit) { case (acc, (err, index)) =>
        acc.flatMap { _ =>
          val req = CreateSubmissionErrorDetailRequest(
            storn                  = storn,
            returnResourceRef      = returnId,
            submissionErrorDetails = SubmissionErrorDetail(
              position     = index.toString,
              errorMessage = err.number.fold(err.text.getOrElse(""))(code => s"$code: ${err.text.getOrElse("")}")
            )
          )
          chrisService.createSubmissionErrorDetail(req).map { _ =>
            logger.info(s"[SubmissionService] error detail persisted number=${err.number.getOrElse("-")} location=${err.location.getOrElse("-")} returnId=$returnId corrId=$correlationId")
            ()
          }
        }
      }

  private def setGovTalkProtocol(storn: String, formResultId: String, protocolStatus: String, correlationId: String)
                                (implicit hc: HeaderCarrier): Future[Unit] =
    val req = UpdateGovTalkStatusRequest(
      userIdentifier    = storn,
      formResultId      = formResultId,
      endStateTimestamp = nowSqlTimestamp,
      protocolStatus    = protocolStatus
    )
    chrisService.updateGovTalkStatus(req).map { _ =>
      logger.info(s"[SubmissionService] GovTalk protocolStatus set to '$protocolStatus' formResultId=$formResultId corrId=$correlationId")
      ()
    }

  private def warnOnCorrelationIdMismatch(storn: String, returnId: String, sent: String, received: Option[String]): Unit =
    received.map(_.trim).filter(id => id.nonEmpty && id != sent).foreach { answered =>
      logger.warn(s"[$logPrefix] correlation id mismatch answered=$answered ${logRef(storn, returnId, sent)}")
    }

  private def updateGovTalkStatistics(storn: String,
                                      formResultId: String,
                                      responseEndPoint: Option[String],
                                      pollIntervalSeconds: Option[Int],
                                      correlationId: String,
                                      numberOfPolls: Int = 0)
                                     (implicit hc: HeaderCarrier): Future[Unit] =
    val gatewayUrl = responseEndPoint.filter(_.nonEmpty).getOrElse(connector.defaultPath)
    val req = UpdateGovTalkStatisticsRequest(
      userIdentifier = storn,
      formResultId   = formResultId,
      govTalkStatus  = GovTalkStatusStatistics(
        lastMessageTimestamp = nowSqlTimestamp,
        numberOfPolls        = numberOfPolls.toString,
        pollInterval         = pollIntervalSeconds.map(_.toString).getOrElse("0"),
        gatewayUrl           = gatewayUrl
      )
    )
    chrisService.updateGovTalkStatistics(req).map { _ =>
      logger.info(s"[SubmissionService] GovTalk statistics updated gatewayUrl=$gatewayUrl pollInterval=${pollIntervalSeconds.map(_.toString).getOrElse("0")} formResultId=$formResultId corrId=$correlationId")
      ()
    }

  // Returns true when the ChRIS resource was deleted (or was already gone), false on any error.
  private def sendChrisDelete(storn: String, returnId: String, endpoint: Option[String], correlationId: String)
                             (implicit hc: HeaderCarrier): Future[Boolean] =
    logger.info(s"[SubmissionService] sending ChRIS DELETE returnId=$returnId corrId=$correlationId endpoint=${endpoint.getOrElse("<default>")}")
    connector.delete(endpoint, correlationId).map {
      case ChrisDeleteResponse.Deleted(_, _) =>
        logger.info(s"[SubmissionService] ChRIS resource DELETED returnId=$returnId corrId=$correlationId")
        true
      case ChrisDeleteResponse.NotFound(_, _) =>
        logger.info(s"[SubmissionService] ChRIS resource already gone (2000) returnId=$returnId corrId=$correlationId")
        true
      case ChrisDeleteResponse.Errored(errors, _, _) =>
        logger.warn(s"[SubmissionService] ChRIS DELETE returned errors (suppressed) returnId=$returnId corrId=$correlationId: ${errors.mkString("; ")}")
        false
      case ChrisDeleteResponse.TransportError(msg, _) =>
        logger.warn(s"[SubmissionService] ChRIS DELETE transport error (suppressed) returnId=$returnId corrId=$correlationId: $msg")
        false
    }.recover { case e =>
      logger.warn(s"[SubmissionService] ChRIS DELETE failed (suppressed) returnId=$returnId corrId=$correlationId: ${e.getMessage}")
      false
    }

  private case class SubmissionContext(storn: String,
                                       returnId: String,
                                       version: Int,
                                       credentialIdentifier: String,
                                       submissionId: String = ""):
    def formResultId: String = submissionId

  private def requireContext(fullReturn: FullReturn, credentialIdentifier: String): Either[String, SubmissionContext] =
    (fullReturn.stornId, fullReturn.returnResourceRef, fullReturn.returnInfo.flatMap(_.version)) match
      case _ if credentialIdentifier.trim.isEmpty =>
        Left("Missing credentialIdentifier; cannot submit. Auth context did not provide a credential ID.")
      case (Some(storn), Some(returnId), Some(version)) =>
        Right(SubmissionContext(storn, returnId, version.toInt, credentialIdentifier))
      case _ =>
        Left(s"FullReturn missing one of stornId / returnResourceRef / returnInfo.version; cannot submit. " +
          s"stornId=${fullReturn.stornId}, returnResourceRef=${fullReturn.returnResourceRef}, " +
          s"version=${fullReturn.returnInfo.flatMap(_.version)}")

  private def newCorrelationId(): String = UUID.randomUUID().toString.replace("-", "")

  private def nowIso: String =
    ZonedDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  private val SqlTimestampFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  private def nowSqlTimestamp: String =
    ZonedDateTime.now(ZoneOffset.UTC).format(SqlTimestampFormatter)

  private val logPrefix: String = "SubmissionService"

  private def logRef(storn: String, returnId: String, correlationId: String): String =
    s"storn=$storn returnId=$returnId corrId=$correlationId"

  private def buildLockRequest(storn: String, formResultId: String, formLockOld: String, formLockNew: String, pollInterval: String = "0", gatewayUrl: Option[String] = None): UpdateGovTalkStatusLockRequest =
    UpdateGovTalkStatusLockRequest(
      userIdentifier = storn,
      formResultId   = formResultId,
      govTalkStatus  = GovTalkStatusLock(
        formLockOld  = formLockOld,
        formLockNew  = formLockNew,
        pollInterval = pollInterval,
        gatewayUrl   = gatewayUrl.filter(_.nonEmpty).getOrElse(connector.defaultPath)
      )
    )

  private def pollIntervalOf(row: SelectGovTalkStatusResponse): String =
    row.pollInterval.flatMap(s => Try(s.trim.toLong).toOption).getOrElse(0L).toString

  private def sendChrisPoll(storn: String, returnId: String, endpoint: Option[String], correlationId: String)
                           (implicit hc: HeaderCarrier): Future[ChrisResponse] =
    logger.info(s"[$logPrefix] sending ChRIS POLL ${logRef(storn, returnId, correlationId)} endpoint=${endpoint.getOrElse("<default>")}")
    connector.poll(endpoint, correlationId)

  private def completeSuccessfulSubmission(storn: String, returnId: String, formResultId: String, correlationId: String, update: SubmissionUpdate, endpoint: Option[String], numberOfPolls: Int = 0)
                                          (implicit hc: HeaderCarrier): Future[SubmissionUpdate] =
    for
      acc     <- persistUpdate(storn, returnId, update, correlationId)
      _       <- updateGovTalkStatistics(storn, formResultId, endpoint, None, correlationId, numberOfPolls)
      _       <- setGovTalkProtocol(storn, formResultId, "deleteRequest", correlationId)
      deleted <- sendChrisDelete(storn, returnId, endpoint, correlationId)
      _       <- if deleted then finaliseGovTalkStatus(storn, formResultId, correlationId)
                 else {
                   logger.warn(s"[SubmissionService] ChRIS delete unsuccessful; leaving GovTalk at deleteRequest (no endState/reset) returnId=$returnId corrId=$correlationId")
                   Future.unit
                 }
    yield acc

  private def closeDepartmentalGovTalk(storn: String, returnId: String, formResultId: String, correlationId: String, endpoint: Option[String], numberOfPolls: Int = 0)
                                      (implicit hc: HeaderCarrier): Future[Unit] =
    for
      _ <- updateGovTalkStatistics(storn, formResultId, endpoint, None, correlationId, numberOfPolls)
      _ <- setGovTalkProtocol(storn, formResultId, "deleteRequest", correlationId)
      _ <- sendChrisDelete(storn, returnId, endpoint, correlationId)
      _ <- setGovTalkProtocol(storn, formResultId, "endState", correlationId)
    yield ()

  private val pollLogPrefix: String = "PollSubmissionsJob"

  private def pollLogRef(sub: SubmissionForPolling): String =
    s"storn=${sub.storn} ref=${sub.returnResourceRef}"

  def poll(sub: SubmissionForPolling)(implicit hc: HeaderCarrier): Future[PollOutcome] =
    chrisService
      .selectGovTalkStatus(SelectGovTalkStatusRequest(sub.storn, sub.submissionId))
      .flatMap { row =>
        resolveCorrelationId(sub, row) match
          case None                => Future.successful(notPolled(sub))
          case Some(correlationId) => pollWithLock(sub, row, correlationId)
      }
      .recover { case e =>
        logger.warn(s"[$pollLogPrefix] failed to poll submission ${sub.returnResourceRef}: ${e.getMessage}")
        notPolled(sub)
      }

  private def resolveCorrelationId(sub: SubmissionForPolling, row: SelectGovTalkStatusResponse): Option[String] =
    row.correlationId.map(_.trim).filter(c => c.nonEmpty && !c.equalsIgnoreCase("empty")) match
      case None =>
        logger.warn(s"[$pollLogPrefix] no correlation id on GovTalk status, skipping ${pollLogRef(sub)}")
        None
      case Some(_) if !(row.protocolStatus.map(_.trim).contains("dataPoll") && intervalElapsed(row)) =>
        logger.info(s"[$pollLogPrefix] poll not allowed yet ${pollLogRef(sub)} protocolStatus=${row.protocolStatus.getOrElse("-")} lastMessage=${row.lastMessageTimestamp.getOrElse("-")} pollInterval=${row.pollInterval.getOrElse("-")}")
        None
      case some => some

  private def intervalElapsed(row: SelectGovTalkStatusResponse): Boolean =
    val interval = pollIntervalOf(row).toLong
    row.lastMessageTimestamp.map(_.trim).filter(_.nonEmpty).flatMap(parseTimestamp) match
      case Some(lastMessage) =>
        !LocalDateTime.now(clock.withZone(ZoneOffset.UTC)).isBefore(lastMessage.plusSeconds(interval))
      case None              => true

  private val TimestampFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private def parseTimestamp(value: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(value.replace('T', ' '), TimestampFormatter)).toOption match
      case None =>
        logger.warn(s"[$pollLogPrefix] bad lastMessage timestamp '$value', treating interval as elapsed")
        None
      case parsed => parsed

  private def pollWithLock(sub: SubmissionForPolling, row: SelectGovTalkStatusResponse, correlationId: String)
    (implicit hc: HeaderCarrier): Future[PollOutcome] =
    acquirePollGovTalkLock(sub, row).flatMap {
      case false => Future.successful(notPolled(sub))
      case true  =>
        pollLocked(sub, row, correlationId).transformWith { result =>
          releaseGovTalkLock(sub.storn, sub.submissionId, sub.returnResourceRef, correlationId, Some(row))
            .transformWith(_ => Future.fromTry(result))
        }
    }

  private def acquirePollGovTalkLock(sub: SubmissionForPolling, row: SelectGovTalkStatusResponse)
                                     (implicit hc: HeaderCarrier): Future[Boolean] =
    chrisService
      .updateGovTalkStatusLock(buildLockRequest(
        sub.storn, sub.submissionId, formLockOld = "N", formLockNew = "Y", pollIntervalOf(row), row.gatewayUrl
      ))
      .map(_ => true)
      .recover { case e =>
        logger.warn(s"[$pollLogPrefix] could not acquire the GovTalk row lock, skipping ${pollLogRef(sub)}: ${e.getMessage}")
        false
      }

  private case class PollContext(
    sub: SubmissionForPolling,
    correlationId: String,
    fullReturn: FullReturn,
    polls: Int,
    rowPollInterval: String,
    gatewayUrl: Option[String]
  ) {
    val storn: String        = sub.storn
    val returnId: String     = sub.returnResourceRef
    val formResultId: String = sub.submissionId
  }

  private def pollLocked(sub: SubmissionForPolling, row: SelectGovTalkStatusResponse, correlationId: String)
    (implicit hc: HeaderCarrier): Future[PollOutcome] =
    for
      fullReturn <- filingConnector.getFullReturn(GetReturnByRefRequest(sub.returnResourceRef, sub.storn))
      ctx         = PollContext(
                      sub             = sub,
                      correlationId   = correlationId,
                      fullReturn      = fullReturn,
                      polls           = row.numberOfPolls.flatMap(n => Try(n.trim.toInt).toOption).getOrElse(0) + 1,
                      rowPollInterval = pollIntervalOf(row),
                      gatewayUrl      = row.gatewayUrl
                    )
      _          <- updateGovTalkStatistics(ctx.storn, ctx.formResultId, ctx.gatewayUrl,
                      Try(ctx.rowPollInterval.toInt).toOption, ctx.correlationId, ctx.polls)
      resp       <- sendChrisPoll(ctx.storn, ctx.returnId, ctx.gatewayUrl, ctx.correlationId)
      outcome    <- handlePollResponse(ctx, resp)
    yield outcome

  private def handlePollResponse(ctx: PollContext, resp: ChrisResponse)(implicit hc: HeaderCarrier): Future[PollOutcome] =
    resp match
      case t: ChrisResponse.TransportError =>
        logger.warn(s"[$pollLogPrefix] no response from ChRIS, retry next cycle cause=${t.message} ${pollLogRef(ctx.sub)}")
        Future.successful(notPolled(ctx.sub))

      case a: ChrisResponse.Acknowledged =>
        pollAcknowledgementBranch(ctx, a)

      case c: ChrisResponse.Completed =>
        pollSuccessBranch(ctx, c, universalStatus(ctx, c))

      case e: ChrisResponse.Errored =>
        pollErrorBranch(ctx, e, universalStatus(ctx, e))

  private def universalStatus(ctx: PollContext, resp: ChrisResponse): UniversalStatus =
    UniversalStatus.fromChrisResponse(resp, ctx.fullReturn.submission.flatMap(_.irmarkSent))

  private def pollAcknowledgementBranch(ctx: PollContext, resp: ChrisResponse.Acknowledged)
                                        (implicit hc: HeaderCarrier): Future[PollOutcome] =
    val nextInterval = resp.pollIntervalSeconds.map(_.toString).getOrElse(ctx.rowPollInterval)
    val nextEndpoint = resp.responseEndPoint.orElse(ctx.gatewayUrl)

    for
      _ <- persistUpdate(ctx.storn, ctx.returnId,
             baseUpdate(ctx.fullReturn).copy(submittableStatus = Some(UniversalStatus.ACCEPTED.toString)),
             ctx.correlationId)
      _ <- setGovTalkProtocol(ctx.storn, ctx.formResultId, "dataPoll", ctx.correlationId)
      _  = warnOnCorrelationIdMismatch(ctx.storn, ctx.returnId, ctx.correlationId, resp.correlationId)
      _ <- updateGovTalkStatistics(ctx.storn, ctx.formResultId, nextEndpoint,
             Try(nextInterval.toInt).toOption, ctx.correlationId, ctx.polls)
    yield polled(ctx, UniversalStatus.ACCEPTED.toString, UniversalStatus.ACCEPTED.toString)

  private def pollSuccessBranch(ctx: PollContext, resp: ChrisResponse.Completed, universal: UniversalStatus)
    (implicit hc: HeaderCarrier): Future[PollOutcome] =
    val endpoint = resp.responseEndPoint.orElse(ctx.gatewayUrl)

    logger.info(s"[$pollLogPrefix] poll SUCCESS ${pollLogRef(ctx.sub)} universalStatus=$universal utrn=${resp.utrn.getOrElse("-")}")
    for
      _       <- completeSuccessfulSubmission(
                   ctx.storn,
                   ctx.returnId,
                   ctx.formResultId,
                   ctx.correlationId,
                   baseUpdate(ctx.fullReturn).copy(
                     submittableStatus = Some(universal.toString),
                     IRMarkRecieved    = resp.receivedIrMark,
                     utrn              = resp.utrn,
                     acceptedDate      = Some(resp.acceptedTime.getOrElse(nowIso))
                   ),
                   endpoint,
                   ctx.polls
                 )
      _        = if resp.utrn.isEmpty then
                   logger.warn(s"[$pollLogPrefix] UC 1.44 AF11: The UTRN is not present in the Submission Response ${pollLogRef(ctx.sub)}")
      _       <- emailService.submitEmailConfirmation(ctx.fullReturn, resp.utrn.getOrElse(""), None).recover { case e =>
                   logger.warn(s"[$pollLogPrefix] confirmation email failed ${pollLogRef(ctx.sub)}: ${e.getMessage}")
                 }
      _       <- audit.auditSubmission(ctx.storn, ctx.returnId, ctx.correlationId, ctx.fullReturn, resp)
    yield polled(ctx, universal.toString, universal.toString)

  private def pollErrorBranch(ctx: PollContext, resp: ChrisResponse.Errored, universal: UniversalStatus)
    (implicit hc: HeaderCarrier): Future[PollOutcome] =
    val deptError   = universal == UniversalStatus.DEPARTMENTAL_ERROR
    val errorStatus = if deptError then UniversalStatus.DEPARTMENTAL_ERROR else UniversalStatus.FATAL_ERROR
    val endpoint    = resp.responseEndPoint.orElse(ctx.gatewayUrl)
    val firstError  = resp.errors.headOption

    logger.warn(s"[$pollLogPrefix] poll ERROR ${pollLogRef(ctx.sub)} universalStatus=$errorStatus departmental=$deptError numbers=${resp.errors.flatMap(_.number).mkString(",")}")
    if deptError then
      logger.warn(s"[$pollLogPrefix] The return is not validated by the HMRC Backend due to Business Validation Rules (BVR) Errors ${pollLogRef(ctx.sub)}")
    else
      logger.warn(s"[$pollLogPrefix] The submission failed due to fatal errors from the Government Gateway ${pollLogRef(ctx.sub)}")

    for
      acc1        <- persistUpdate(ctx.storn, ctx.returnId,
                       baseUpdate(ctx.fullReturn).copy(submittableStatus = Some(errorStatus.toString)),
                       ctx.correlationId)
      _           <- if deptError then closeDepartmentalGovTalk(ctx.storn, ctx.returnId,
                       ctx.formResultId, ctx.correlationId, endpoint, ctx.polls)
                     else Future.unit
      acc2        <- persistUpdate(ctx.storn, ctx.returnId, acc1.copy(
                       govTalkErrorCode    = firstError.flatMap(_.number),
                       govTalkErrorType    = firstError.map(_.classification),
                       govTalkErrorMessage = firstError.flatMap(_.text)
                     ), ctx.correlationId)
      finalStatus <- recoverableTail(ctx.storn, ctx.returnId, acc2, resp.errors, ctx.correlationId)
                       .map(_.fold(errorStatus.toString)(_ => UniversalStatus.STARTED.toString))
      _           <- createSubmissionErrorDetails(ctx.storn, ctx.returnId, resp.errors, ctx.correlationId)
      _           <- audit.auditSubmission(ctx.storn, ctx.returnId, ctx.correlationId, ctx.fullReturn, resp)
    yield polled(ctx, errorStatus.toString, finalStatus)

  private def baseUpdate(fullReturn: FullReturn): SubmissionUpdate = {
    val existing = fullReturn.submission
    SubmissionUpdate(
      IRMarkRecieved        = existing.flatMap(_.irmarkReceived),
      utrn                  = existing.flatMap(_.UTRN),
      email                 = existing.flatMap(_.email),
      submissionRequestDate = existing.flatMap(_.submissionRequestDate),
      acceptedDate          = existing.flatMap(_.acceptedDate),
      submittableStatus     = existing.flatMap(_.submissionStatus),
      govTalkErrorCode      = existing.flatMap(_.govtalkErrorCode),
      govTalkErrorType      = existing.flatMap(_.govtalkErrorType),
      govTalkErrorMessage   = existing.flatMap(_.govtalkErrorMessage),
      IRMarkSent            = existing.flatMap(_.irmarkSent)
    )
  }

  private def notPolled(sub: SubmissionForPolling): PollOutcome =
    PollOutcome(sub, polled = false, pollResult = "-",
      newReturnStatus = "-", correlationId = "(not polled)")

  private def polled(ctx: PollContext, pollResult: String, newReturnStatus: String): PollOutcome =
    PollOutcome(ctx.sub, polled = true, pollResult = pollResult,
      newReturnStatus = newReturnStatus, correlationId = ctx.correlationId)
