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

package models.submission

import play.api.libs.json.{Json, OFormat, OWrites, Reads}

case class GovTalkStatusReturn(success: Boolean)

object GovTalkStatusReturn {
  implicit val format: OFormat[GovTalkStatusReturn] = Json.format[GovTalkStatusReturn]
}


case class GovTalkStatusInitial(
                                 formLock: String,
                                 createTimestamp: String,
                                 endStateTimestamp: Option[String],
                                 lastMessageTimestamp: String,
                                 numberOfPolls: String,
                                 pollInterval: String,
                                 protocolStatus: String,
                                 gatewayUrl: String
                               )

object GovTalkStatusInitial {
  implicit val format: OFormat[GovTalkStatusInitial] = Json.format[GovTalkStatusInitial]
}

case class GovTalkStatusReset(
                               formLock: String,
                               createTimestamp: String,
                               endStateTimestamp: Option[String],
                               lastMessageTimestamp: String,
                               numberOfPolls: String,
                               pollInterval: String,
                               protocolStatusOld: String,
                               protocolStatusNew: String,
                               gatewayUrl: String
                             )

object GovTalkStatusReset {
  implicit val format: OFormat[GovTalkStatusReset] = Json.format[GovTalkStatusReset]
}

case class GovTalkStatusLock(
                              formLockOld: String,
                              formLockNew: String,
                              pollInterval: String,
                              gatewayUrl: String
                            )

object GovTalkStatusLock {
  implicit val format: OFormat[GovTalkStatusLock] = Json.format[GovTalkStatusLock]
}

case class GovTalkStatusStatistics(
                                    lastMessageTimestamp: String,
                                    numberOfPolls: String,
                                    pollInterval: String,
                                    gatewayUrl: String
                                  )

object GovTalkStatusStatistics {
  implicit val format: OFormat[GovTalkStatusStatistics] = Json.format[GovTalkStatusStatistics]
}

case class InsertInitialGovTalkStatusRequest(
                                              userIdentifier: String,
                                              formResultId: String,
                                              correlationId: String,
                                              govTalkStatus: GovTalkStatusInitial
                                            )

object InsertInitialGovTalkStatusRequest {
  implicit val format: OFormat[InsertInitialGovTalkStatusRequest] = Json.format[InsertInitialGovTalkStatusRequest]
}

case class ResetGovTalkStatusRequest(
                                      userIdentifier: String,
                                      formResultId: String,
                                      correlationId: String,
                                      govTalkStatus: GovTalkStatusReset
                                    )

object ResetGovTalkStatusRequest {
  implicit val format: OFormat[ResetGovTalkStatusRequest] = Json.format[ResetGovTalkStatusRequest]
}

case class UpdateGovTalkStatusRequest(
                                       userIdentifier: String,
                                       formResultId: String,
                                       endStateTimestamp: String,
                                       protocolStatus: String
                                     )

object UpdateGovTalkStatusRequest {
  implicit val format: OFormat[UpdateGovTalkStatusRequest] = Json.format[UpdateGovTalkStatusRequest]
}

case class UpdateGovTalkStatusCorrelationIdRequest(
                                                    userIdentifier: String,
                                                    formResultId: String,
                                                    correlationId: String,
                                                    pollInterval: Int,
                                                    gatewayUrl: String
                                                  )

object UpdateGovTalkStatusCorrelationIdRequest {
  implicit val format: OFormat[UpdateGovTalkStatusCorrelationIdRequest] = Json.format[UpdateGovTalkStatusCorrelationIdRequest]
}

case class UpdateGovTalkStatusLockRequest(
                                           userIdentifier: String,
                                           formResultId: String,
                                           govTalkStatus: GovTalkStatusLock
                                         )

object UpdateGovTalkStatusLockRequest {
  implicit val format: OFormat[UpdateGovTalkStatusLockRequest] = Json.format[UpdateGovTalkStatusLockRequest]
}

case class UpdateGovTalkStatisticsRequest(
                                           userIdentifier: String,
                                           formResultId: String,
                                           govTalkStatus: GovTalkStatusStatistics
                                         )

object UpdateGovTalkStatisticsRequest {
  implicit val format: OFormat[UpdateGovTalkStatisticsRequest] = Json.format[UpdateGovTalkStatisticsRequest]
}

case class DeleteGovTalkStatusRequest(
                                       resultId: String
                                     )

object DeleteGovTalkStatusRequest {
  implicit val format: OFormat[DeleteGovTalkStatusRequest] = Json.format[DeleteGovTalkStatusRequest]
}


case class SelectGovTalkStatusRequest(
                                       userIdentifier: String,
                                       formResultId: String
                                     )

object SelectGovTalkStatusRequest {
  implicit val format: OFormat[SelectGovTalkStatusRequest] = Json.format[SelectGovTalkStatusRequest]
}

case class SelectGovTalkFormResultIdRequest(
                                             userIdentifier: String
                                           )

object SelectGovTalkFormResultIdRequest {
  implicit val format: OFormat[SelectGovTalkFormResultIdRequest] = Json.format[SelectGovTalkFormResultIdRequest]
}

case class SelectGovTalkStatusResponse(
                                        userIdentifier: Option[String],
                                        formResultId: Option[String],
                                        correlationId: Option[String],
                                        formLock: Option[String],
                                        createTimestamp: Option[String],
                                        endStateTimestamp: Option[String],
                                        lastMessageTimestamp: Option[String],
                                        numberOfPolls: Option[String],
                                        pollInterval: Option[String],
                                        protocolStatus: Option[String],
                                        gatewayUrl: Option[String]
                                      )

object SelectGovTalkStatusResponse {
  implicit val format: OFormat[SelectGovTalkStatusResponse] = Json.format[SelectGovTalkStatusResponse]
}

case class SelectGovTalkFormResultIdResponse(
                                              formResultId: Option[String]
                                            )

object SelectGovTalkFormResultIdResponse {
  implicit val format: OFormat[SelectGovTalkFormResultIdResponse] = Json.format[SelectGovTalkFormResultIdResponse]
}