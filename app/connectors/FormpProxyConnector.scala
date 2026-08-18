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

package connectors

import models.agent.*
import models.manage.{SdltReturnRecordRequest, SdltReturnRecordResponse}
import models.polling.SubmissionsForPollingResponse
import models.purge.{DeleteReturnRequest, DeleteReturnResponse, GetReturnsForPurgeRequest, ReturnsForPurgeResponse}
import play.api.Logging
import play.api.libs.json.Json
import play.api.libs.ws.JsonBodyWritables.*
import uk.gov.hmrc.http.HttpReads.Implicits.*
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class FormpProxyConnector @Inject()(http: HttpClientV2,
                                    config: ServicesConfig)
                                   (implicit ec: ExecutionContext) extends Logging {

  private val stubPath = config.baseUrl("stamp-duty-land-tax-stub") + "/stamp-duty-land-tax-stub"
  private val formpPath = config.baseUrl("formp-proxy") + "/formp-proxy"
  val stubFormPBool: Boolean = config.getBoolean("features.stub-formp-enabled")
  private val internalAuthToken: String = config.getString("internal-auth.token")

  def submitAgentDetails(createPredefinedAgentRequest: CreatePredefinedAgentRequest)(implicit hc: HeaderCarrier): Future[CreatePredefinedAgentResponse] =
    val url: URL = if(stubFormPBool) url"$stubPath/create/predefined-agent" else url"$formpPath/create/predefined-agent"
    http.post(url)
      .withBody(Json.toJson(createPredefinedAgentRequest))
      .execute[CreatePredefinedAgentResponse]
      .recover {
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][submitAgentDetails]: ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
      }

  def getSdltOrganisation(storn: String)(implicit hc: HeaderCarrier): Future[SdltOrganisationResponse] =
    val url: URL = if(stubFormPBool) url"$stubPath/organisation" else url"$formpPath/organisation"
    http.post(url)
      .withBody(Json.obj("storn" -> storn))
      .execute[SdltOrganisationResponse]
      .recover {
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][getSdltOrganisation]: ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
      }

  def deletePredefinedAgent(deletePredefinedAgentRequest: DeletePredefinedAgentRequest)
                 (implicit hc: HeaderCarrier): Future[DeletePredefinedAgentResponse] =
    val url: URL = if(stubFormPBool) url"$stubPath/delete/predefined-agent" else url"$formpPath/delete/predefined-agent"
    http.post(url)
      .withBody(Json.toJson(deletePredefinedAgentRequest))
      .execute[DeletePredefinedAgentResponse]
      .recover {
        case e: UpstreamErrorResponse =>
          logger.error(s"[FormpProxyConnector][deletePredefinedAgent]: Upstream error - ${e.getMessage}")
          throw e
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][deletePredefinedAgent]: ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
      }

  def getReturns(request: SdltReturnRecordRequest)
                (implicit hc: HeaderCarrier): Future[SdltReturnRecordResponse] = {
    val url: URL = if (stubFormPBool) url"$stubPath/returns" else url"$formpPath/returns"
    http.post(url)
      .withBody(Json.toJson(request))
      .execute[Either[UpstreamErrorResponse, SdltReturnRecordResponse]]
      .flatMap {
        case Right(resp) =>
          logger.info(s"[FormpProxyConnector][getReturns] - ${request}::response r/count: ${resp.returnSummaryCount}/${resp.returnSummaryList.length}")
          Future.successful(resp)
        case Left(error) =>
          logger.error(s"[FormpProxyConnector][getReturns] - ${request.storn}::response errror: ${error}")
          Future.failed(error)
      }
      .recoverWith {
        case NonFatal(e) =>
          logger.error(s"[FormpProxyConnector][getReturns] failed for storn ${request.storn}: ${e.getMessage}", e)
          Future.failed(e)
      }
  }
  def updateAgentDetails(updateAgentDetails: UpdatePredefinedAgentRequest)(implicit hc: HeaderCarrier): Future[UpdatePredefinedAgentResponse] =
    val url: URL = if (stubFormPBool) url"$stubPath/update/predefined-agent" else url"$formpPath/update/predefined-agent"
    http.post(url)
      .withBody(Json.toJson(updateAgentDetails))
      .execute[UpdatePredefinedAgentResponse]
      .recover {
        case e: UpstreamErrorResponse =>
          logger.error(s"[FormpProxyConnector][updatePredefinedAgent]: Upstream error - ${e.getMessage}")
          throw e
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][UpdateAgent]: ${e.getMessage} the payload is:\n \n ${Json.toJson(updateAgentDetails)}")
          throw new RuntimeException(e.getMessage)
      }

  def getReturnsForPurge(request: GetReturnsForPurgeRequest)(implicit hc: HeaderCarrier): Future[ReturnsForPurgeResponse] =
    val url: URL = if (stubFormPBool) url"$stubPath/returns-for-purge" else url"$formpPath/returns-for-purge"
    http.post(url)
      .setHeader("Authorization" -> internalAuthToken)
      .withBody(Json.toJson(request))
      .execute[ReturnsForPurgeResponse]
      .recover {
        case e: UpstreamErrorResponse =>
          logger.error(s"[FormpProxyConnector][getReturnsForPurge]: Upstream error ${e.statusCode} - ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][getReturnsForPurge]: ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
      }

  def getSubmissionsForPolling()(implicit hc: HeaderCarrier): Future[SubmissionsForPollingResponse] =
    val url: URL = if (stubFormPBool) url"$stubPath/submissions-polling" else url"$formpPath/submissions-polling"
    http.get(url)
      .setHeader("Authorization" -> internalAuthToken)
      .execute[SubmissionsForPollingResponse]
      .recover {
        case e: UpstreamErrorResponse if e.statusCode == 401 =>
          logger.error(s"[FormpProxyConnector][getSubmissionsForPolling]: 401 Unauthorized from formp-proxy")
          throw new RuntimeException(e.getMessage)
        case e: UpstreamErrorResponse =>
          logger.error(s"[FormpProxyConnector][getSubmissionsForPolling]: Upstream error ${e.statusCode} - ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][getSubmissionsForPolling]: ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
      }

  def deleteReturn(request: DeleteReturnRequest)(implicit hc: HeaderCarrier): Future[DeleteReturnResponse] =
    val url: URL = if (stubFormPBool) url"$stubPath/delete/return" else url"$formpPath/delete/return"
    http.post(url)
      .setHeader("Authorization" -> internalAuthToken)
      .withBody(Json.toJson(request))
      .execute[DeleteReturnResponse]
      .recover {
        case e: UpstreamErrorResponse =>
          logger.error(s"[FormpProxyConnector][deleteReturn]: Upstream error ${e.statusCode} - ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
        case e: Throwable =>
          logger.error(s"[FormpProxyConnector][deleteReturn]: ${e.getMessage}")
          throw new RuntimeException(e.getMessage)
      }

}
