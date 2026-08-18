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

package scheduler

import org.apache.pekko.actor.{Actor, ActorLogging, Props}
import play.api.Logging
import scheduler.SchedulingActor.*
import service.{PollSubmissionsService, PurgeReturnsService}

class SchedulingActor extends Actor with ActorLogging with Logging {

  override def receive: Receive = {
    case message: ScheduledMessage[?] =>
      logger.info(s"Received ${message.getClass.getSimpleName}")
      message.service.invoke
  }
}

object MongoLockKeys {
  val purgeReturnsLock: String    = "PurgeReturnsJob"
  val pollSubmissionsLock: String = "PollSubmissionsJob"
}

object SchedulingActor {

  sealed trait ScheduledMessage[A] {
    val service: ScheduledService[A]
  }

  def props: Props = Props[SchedulingActor]()

  case class PurgeReturns(service: PurgeReturnsService)
      extends ScheduledMessage[Either[ScheduleStatus.JobFailed, List[String]]]

  case class PollSubmissions(service: PollSubmissionsService)
      extends ScheduledMessage[Either[ScheduleStatus.JobFailed, List[String]]]
}
