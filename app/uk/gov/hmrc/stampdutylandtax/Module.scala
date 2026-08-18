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

package uk.gov.hmrc.stampdutylandtax

import play.api.inject.{Binding, Module as AppModule}
import play.api.{Configuration, Environment}
import scheduler.jobs.{PollSubmissionsJob, PurgeReturnsJob}
import uk.gov.hmrc.stampdutylandtax.controllers.actions.{AuthenticatedIdentifierAction, IdentifierAction}

import java.time.Clock

class Module extends AppModule:

  override def bindings(
    environment  : Environment,
    configuration: Configuration
  ): Seq[Binding[_]] = {
      bind[Clock].toInstance(Clock.systemDefaultZone) :: // inject if current time needs to be controlled in unit tests
      bind[IdentifierAction].to(classOf[AuthenticatedIdentifierAction]) :: // TODO: clarify how to change instantiation level :: asEagerSingleton() ??
      bind[PurgeReturnsJob].toSelf.eagerly() ::
      bind[PollSubmissionsJob].toSelf.eagerly() ::
      Nil
  }