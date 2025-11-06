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

import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import service.ManageReturnsService
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton()
class ManageReturnsController @Inject()(
  cc: ControllerComponents,
  service: ManageReturnsService
)(implicit ec: ExecutionContext) extends BackendController(cc) with Logging:

  def getReturns(storn: String): Action[AnyContent] = Action.async { implicit request =>
    service.getReturns(storn)
      .map {
        case Some(returnItem) => Ok(Json.toJson(returnItem))
        case None             => NotFound(Json.obj("message" -> s"No returns found for storn: $storn"))
    } recover {
      case u: UpstreamErrorResponse =>
        logger.error("[ManageReturnsController][getReturns] failed with UpstreamErrorResponse", u)
        Status(u.statusCode)(Json.obj("message" -> u.message))
      case t: Throwable =>
        logger.error("[ManageReturnsController][getReturns] failed", t)
        InternalServerError(Json.obj("message" -> "Unexpected error"))
    }
  }
