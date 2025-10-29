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

package base

import models.{AgentDetailsAfterCreation, AgentDetailsBeforeCreation}
import models.response.SubmitAgentDetailsResponse
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.{BaseOneAppPerSuite, FakeApplicationFactory}
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{AnyContentAsEmpty, ControllerComponents, PlayBodyParsers}
import play.api.test.{DefaultAwaitTimeout, FakeRequest}
import play.api.test.Helpers.stubControllerComponents
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait SpecBase
  extends AnyFreeSpec
    with Matchers
    with DefaultAwaitTimeout
    with ScalaFutures
    with FakeApplicationFactory
    with BaseOneAppPerSuite
    with MockitoSugar 
    with BeforeAndAfterEach{

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.auth.host" -> "localhost",
        "microservice.services.auth.port" -> 11111,
        "microservice.services.rds-datacache-proxy.host" -> "localhost",
        "microservice.services.rds-datacache-proxy.port" -> 11111
      )
      .build()

  val testAgentDetailsBeforeCreation: AgentDetailsBeforeCreation = AgentDetailsBeforeCreation(
    agentName = "Acme Property Agents Ltd",
    houseNumber = "64",
    addressLine1 = "Zoo Lane",
    addressLine2 = Some("Westminster"),
    addressLine3 = "London",
    addressLine4 = None,
    postcode = Some("SW1A 2AA"),
    phone = Some("02079460000"),
    email = "test@example.com"
  )

  val testAgentDetailsAfterCreation: AgentDetailsAfterCreation = AgentDetailsAfterCreation(
    agentReferenceNumber = "ARN001",
    agentName = "Acme Property Agents Ltd",
    houseNumber = "64",
    addressLine1 = "Zoo Lane",
    addressLine2 = Some("Westminster"),
    addressLine3 = "London",
    addressLine4 = None,
    postcode = Some("SW1A 2AA"),
    phone = Some("02079460000"),
    email = "test@example.com"
  )

  val testAgentDetailsList: List[AgentDetailsAfterCreation] = List(
    AgentDetailsAfterCreation(
      agentReferenceNumber = "ARN001",
      agentName = "Acme Property Agents Ltd",
      houseNumber = "64",
      addressLine1 = "Zoo Lane",
      addressLine2 = Some("Westminster"),
      addressLine3 = "London",
      addressLine4 = None,
      postcode = Some("SW1A 2AA"),
      phone = Some("02079460000"),
      email = "test@example.com"
    ),
    AgentDetailsAfterCreation(
      agentReferenceNumber = "ARN001",
      agentName = "BrightHomes Estates",
      houseNumber = "12B",
      addressLine1 = "Maple Street",
      addressLine2 = Some("Camden"),
      addressLine3 = "London",
      addressLine4 = Some("Greater London"),
      postcode = Some("NW1 5LE"),
      phone = Some("02071234567"),
      email = "info@brighthomes.co.uk"
    )
  )

  val testAgentDetailsSuccessResponse: SubmitAgentDetailsResponse = SubmitAgentDetailsResponse(
    agentResourceRef = "some-id"
  )

  val cc: ControllerComponents = stubControllerComponents()
  val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()
  val bodyParsers: PlayBodyParsers = app.injector.instanceOf[PlayBodyParsers]

  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val ec: ExecutionContext = cc.executionContext

}