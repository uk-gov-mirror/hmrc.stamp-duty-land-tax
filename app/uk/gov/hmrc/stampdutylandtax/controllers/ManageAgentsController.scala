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

package uk.gov.hmrc.stampdutylandtax.controllers

import models.{AgentDetailsAfterCreation, AgentDetailsBeforeCreation}
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import service.ManageAgentsService
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton()
class ManageAgentsController @Inject()(
  cc: ControllerComponents,
  service: ManageAgentsService
)(implicit ec: ExecutionContext) extends BackendController(cc) with Logging:

  def getAgentDetails(storn: String, agentReferenceNumber: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAgentDetails(storn, agentReferenceNumber)
      .map {
        case Some(agentDetails) => Ok(Json.toJson(agentDetails))
        case None               => NotFound(Json.obj("message" -> "Agent details not found"))
    } recover {
      case u: UpstreamErrorResponse =>
        Status(u.statusCode)(Json.obj("message" -> u.message))
      case t: Throwable =>
        logger.error("[getAgentDetails] failed", t)
        InternalServerError(Json.obj("message" -> "Unexpected error"))
    }
  }

  def getAllAgents(storn: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAllAgents(storn) map { agentDetailsList =>
      Ok(Json.toJson(
        agentDetailsList
      ))
    } recover {
      case u: UpstreamErrorResponse =>
        Status(u.statusCode)(Json.obj("message" -> u.message))
      case t: Throwable =>
        logger.error("[getAllAgents] failed", t)
        InternalServerError(Json.obj("message" -> "Unexpected error"))
    }
  }

  def removeAgent(storn: String, agentReferenceNumber: String): Action[AnyContent] = Action.async { implicit request =>
    service.removeAgent(storn, agentReferenceNumber) map { isRemoved =>
      Ok(Json.toJson(
        isRemoved
      ))
    } recover {
      case u: UpstreamErrorResponse =>
        Status(u.statusCode)(Json.obj("message" -> u.message))
      case t: Throwable =>
        logger.error("[removeAgent] failed", t)
        InternalServerError(Json.obj("message" -> "Unexpected error"))
    }
  }

  def submitAgentDetails: Action[JsValue] = Action.async(parse.json) { implicit request =>
    request.body.validate[AgentDetailsBeforeCreation].fold(
      invalid => Future.successful(BadRequest(Json.obj("message" -> s"Invalid payload: $invalid"))),
      payload =>
        service.submitAgentDetails(payload) map { submissionResponse =>
          Ok(Json.toJson(
            submissionResponse
          ))
        } recover {
          case u: UpstreamErrorResponse =>
            Status(u.statusCode)(Json.obj("message" -> u.message))
          case t: Throwable =>
            logger.error("[getAgentDetails] failed", t)
            InternalServerError(Json.obj("message" -> "Unexpected error"))
        }
    )
  }
