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

package service

import connectors.FormpProxyConnector
import models.{AgentDetailsAfterCreation, AgentDetailsBeforeCreation}
import models.response.SubmitAgentDetailsResponse
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

@Singleton
class ManageAgentsService @Inject()(formp: FormpProxyConnector) {

  def getAgentDetails(storn: String, agentReferenceNumber: String)
                     (implicit hc: HeaderCarrier): Future[Option[AgentDetailsAfterCreation]] =
    formp.getAgentDetails(storn, agentReferenceNumber)

  def submitAgentDetails(agentDetails: AgentDetailsBeforeCreation)
                        (implicit hc: HeaderCarrier): Future[SubmitAgentDetailsResponse] =
    formp.submitAgentDetails(agentDetails)

  def getAllAgents(storn: String)
                  (implicit hc: HeaderCarrier): Future[List[AgentDetailsAfterCreation]] =
    formp.getAllAgents(storn)

  def removeAgent(storn: String, agentReferenceNumber: String)
                 (implicit hc: HeaderCarrier): Future[Boolean] =
    formp.removeAgent(storn, agentReferenceNumber)
}
