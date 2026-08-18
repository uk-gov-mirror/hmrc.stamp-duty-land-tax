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

package uk.gov.hmrc.stampdutylandtax.models.submission

import models.submission._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsObject, Json}

class GovTalkStatusModelsSpec extends AnyWordSpec with Matchers {

  "GovTalkStatusReturn" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"success": true
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusReturn]
      model.success mustBe true

      Json.toJson(model).as[GovTalkStatusReturn] mustBe model
    }
  }

  "GovTalkStatusInitial" should {
    "read and write fully populated with all optional and mandatory fields" in {
      val json = Json.parse(
        s"""
           |{
           |"formLock":"N",
           |"createTimestamp":"2026-01-02T09:00:00Z",
           |"endStateTimestamp":"2026-01-02T09:05:00Z",
           |"lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |"numberOfPolls":"3",
           |"pollInterval":"10",
           |"protocolStatus":"SUBMITTED",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusInitial]
      model.formLock mustBe "N"
      model.createTimestamp mustBe "2026-01-02T09:00:00Z"
      model.endStateTimestamp mustBe Some("2026-01-02T09:05:00Z")
      model.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.numberOfPolls mustBe "3"
      model.pollInterval mustBe "10"
      model.protocolStatus mustBe "SUBMITTED"
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[GovTalkStatusInitial] mustBe model
    }

    "read and write fully populated with all mandatory fields only" in {
      val json = Json.parse(
        s"""
           |{
           |"formLock":"N",
           |"createTimestamp":"2026-01-02T09:00:00Z",
           |"lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |"numberOfPolls":"3",
           |"pollInterval":"10",
           |"protocolStatus":"SUBMITTED",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusInitial]
      model.formLock mustBe "N"
      model.createTimestamp mustBe "2026-01-02T09:00:00Z"
      model.endStateTimestamp mustBe None
      model.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.numberOfPolls mustBe "3"
      model.pollInterval mustBe "10"
      model.protocolStatus mustBe "SUBMITTED"
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[GovTalkStatusInitial] mustBe model
    }
  }

  "GovTalkStatusReset" should {
    "read and write fully populated with all optional and mandatory fields" in {
      val json = Json.parse(
        s"""
           |{
           |"formLock":"N",
           |"createTimestamp":"2026-01-02T09:00:00Z",
           |"endStateTimestamp":"2026-01-02T09:05:00Z",
           |"lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |"numberOfPolls":"3",
           |"pollInterval":"10",
           |"protocolStatusOld":"SUBMITTED",
           |"protocolStatusNew":"ACKNOWLEDGED",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusReset]
      model.formLock mustBe "N"
      model.createTimestamp mustBe "2026-01-02T09:00:00Z"
      model.endStateTimestamp mustBe Some("2026-01-02T09:05:00Z")
      model.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.numberOfPolls mustBe "3"
      model.pollInterval mustBe "10"
      model.protocolStatusOld mustBe "SUBMITTED"
      model.protocolStatusNew mustBe "ACKNOWLEDGED"
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[GovTalkStatusReset] mustBe model
    }

    "read and write fully populated with all mandatory fields only" in {
      val json = Json.parse(
        s"""
           |{
           |"formLock":"N",
           |"createTimestamp":"2026-01-02T09:00:00Z",
           |"lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |"numberOfPolls":"3",
           |"pollInterval":"10",
           |"protocolStatusOld":"SUBMITTED",
           |"protocolStatusNew":"ACKNOWLEDGED",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusReset]
      model.formLock mustBe "N"
      model.createTimestamp mustBe "2026-01-02T09:00:00Z"
      model.endStateTimestamp mustBe None
      model.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.numberOfPolls mustBe "3"
      model.pollInterval mustBe "10"
      model.protocolStatusOld mustBe "SUBMITTED"
      model.protocolStatusNew mustBe "ACKNOWLEDGED"
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[GovTalkStatusReset] mustBe model
    }
  }

  "GovTalkStatusLock" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"formLockOld":"N",
           |"formLockNew":"Y",
           |"pollInterval":"10",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusLock]
      model.formLockOld mustBe "N"
      model.formLockNew mustBe "Y"
      model.pollInterval mustBe "10"
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[GovTalkStatusLock] mustBe model
    }
  }

  "GovTalkStatusStatistics" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |"numberOfPolls":"3",
           |"pollInterval":"10",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[GovTalkStatusStatistics]
      model.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.numberOfPolls mustBe "3"
      model.pollInterval mustBe "10"
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[GovTalkStatusStatistics] mustBe model
    }
  }

  "InsertInitialGovTalkStatusRequest" should {
    "read and write fully populated with all optional and mandatory fields" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"correlationId":"CORR-1",
           |"govTalkStatus":{
           |  "formLock":"N",
           |  "createTimestamp":"2026-01-02T09:00:00Z",
           |  "endStateTimestamp":"2026-01-02T09:05:00Z",
           |  "lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |  "numberOfPolls":"3",
           |  "pollInterval":"10",
           |  "protocolStatus":"SUBMITTED",
           |  "gatewayUrl":"https://gateway.example/submit"
           |}
           |}
           |""".stripMargin)

      val model = json.as[InsertInitialGovTalkStatusRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.correlationId mustBe "CORR-1"
      model.govTalkStatus.formLock mustBe "N"
      model.govTalkStatus.createTimestamp mustBe "2026-01-02T09:00:00Z"
      model.govTalkStatus.endStateTimestamp mustBe Some("2026-01-02T09:05:00Z")
      model.govTalkStatus.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.govTalkStatus.numberOfPolls mustBe "3"
      model.govTalkStatus.pollInterval mustBe "10"
      model.govTalkStatus.protocolStatus mustBe "SUBMITTED"
      model.govTalkStatus.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[InsertInitialGovTalkStatusRequest] mustBe model
    }

    "read and write fully populated with all mandatory fields only" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"correlationId":"CORR-1",
           |"govTalkStatus":{
           |  "formLock":"N",
           |  "createTimestamp":"2026-01-02T09:00:00Z",
           |  "lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |  "numberOfPolls":"3",
           |  "pollInterval":"10",
           |  "protocolStatus":"SUBMITTED",
           |  "gatewayUrl":"https://gateway.example/submit"
           |}
           |}
           |""".stripMargin)

      val model = json.as[InsertInitialGovTalkStatusRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.correlationId mustBe "CORR-1"
      model.govTalkStatus.endStateTimestamp mustBe None

      Json.toJson(model).as[InsertInitialGovTalkStatusRequest] mustBe model
    }
  }

  "ResetGovTalkStatusRequest" should {
    "read and write fully populated with all optional and mandatory fields" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"correlationId":"CORR-1",
           |"govTalkStatus":{
           |  "formLock":"N",
           |  "createTimestamp":"2026-01-02T09:00:00Z",
           |  "endStateTimestamp":"2026-01-02T09:05:00Z",
           |  "lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |  "numberOfPolls":"3",
           |  "pollInterval":"10",
           |  "protocolStatusOld":"SUBMITTED",
           |  "protocolStatusNew":"ACKNOWLEDGED",
           |  "gatewayUrl":"https://gateway.example/submit"
           |}
           |}
           |""".stripMargin)

      val model = json.as[ResetGovTalkStatusRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.correlationId mustBe "CORR-1"
      model.govTalkStatus.formLock mustBe "N"
      model.govTalkStatus.createTimestamp mustBe "2026-01-02T09:00:00Z"
      model.govTalkStatus.endStateTimestamp mustBe Some("2026-01-02T09:05:00Z")
      model.govTalkStatus.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.govTalkStatus.numberOfPolls mustBe "3"
      model.govTalkStatus.pollInterval mustBe "10"
      model.govTalkStatus.protocolStatusOld mustBe "SUBMITTED"
      model.govTalkStatus.protocolStatusNew mustBe "ACKNOWLEDGED"
      model.govTalkStatus.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[ResetGovTalkStatusRequest] mustBe model
    }

    "read and write fully populated with all mandatory fields only" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"correlationId":"CORR-1",
           |"govTalkStatus":{
           |  "formLock":"N",
           |  "createTimestamp":"2026-01-02T09:00:00Z",
           |  "lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |  "numberOfPolls":"3",
           |  "pollInterval":"10",
           |  "protocolStatusOld":"SUBMITTED",
           |  "protocolStatusNew":"ACKNOWLEDGED",
           |  "gatewayUrl":"https://gateway.example/submit"
           |}
           |}
           |""".stripMargin)

      val model = json.as[ResetGovTalkStatusRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.correlationId mustBe "CORR-1"
      model.govTalkStatus.endStateTimestamp mustBe None

      Json.toJson(model).as[ResetGovTalkStatusRequest] mustBe model
    }
  }

  "UpdateGovTalkStatusRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"endStateTimestamp":"2026-01-02T09:05:00Z",
           |"protocolStatus":"ACKNOWLEDGED"
           |}
           |""".stripMargin)

      val model = json.as[UpdateGovTalkStatusRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.endStateTimestamp mustBe "2026-01-02T09:05:00Z"
      model.protocolStatus mustBe "ACKNOWLEDGED"

      Json.toJson(model).as[UpdateGovTalkStatusRequest] mustBe model
    }
  }

  "UpdateGovTalkStatusCorrelationIdRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"correlationId":"CORR-1",
           |"pollInterval":0,
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[UpdateGovTalkStatusCorrelationIdRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.correlationId mustBe "CORR-1"
      model.pollInterval mustBe 0
      model.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[UpdateGovTalkStatusCorrelationIdRequest] mustBe model
    }

    "write the field names formp-proxy requires" in {
      val model = UpdateGovTalkStatusCorrelationIdRequest(
        userIdentifier = "USER-1",
        formResultId   = "FR-1",
        correlationId  = "CORR-1",
        pollInterval   = 0,
        gatewayUrl     = "https://gateway.example/submit"
      )

      Json.toJson(model).as[JsObject].keys mustBe
        Set("userIdentifier", "formResultId", "correlationId", "pollInterval", "gatewayUrl")
    }
  }

  "UpdateGovTalkStatusLockRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"govTalkStatus":{
           |  "formLockOld":"N",
           |  "formLockNew":"Y",
           |  "pollInterval":"10",
           |  "gatewayUrl":"https://gateway.example/submit"
           |}
           |}
           |""".stripMargin)

      val model = json.as[UpdateGovTalkStatusLockRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.govTalkStatus.formLockOld mustBe "N"
      model.govTalkStatus.formLockNew mustBe "Y"
      model.govTalkStatus.pollInterval mustBe "10"
      model.govTalkStatus.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[UpdateGovTalkStatusLockRequest] mustBe model
    }
  }

  "UpdateGovTalkStatisticsRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"govTalkStatus":{
           |  "lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |  "numberOfPolls":"3",
           |  "pollInterval":"10",
           |  "gatewayUrl":"https://gateway.example/submit"
           |}
           |}
           |""".stripMargin)

      val model = json.as[UpdateGovTalkStatisticsRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"
      model.govTalkStatus.lastMessageTimestamp mustBe "2026-01-02T09:04:00Z"
      model.govTalkStatus.numberOfPolls mustBe "3"
      model.govTalkStatus.pollInterval mustBe "10"
      model.govTalkStatus.gatewayUrl mustBe "https://gateway.example/submit"

      Json.toJson(model).as[UpdateGovTalkStatisticsRequest] mustBe model
    }
  }

  "DeleteGovTalkStatusRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"resultId":"FR-1"
           |}
           |""".stripMargin)

      val model = json.as[DeleteGovTalkStatusRequest]
      model.resultId mustBe "FR-1"

      Json.toJson(model).as[DeleteGovTalkStatusRequest] mustBe model
    }
  }

  "SelectGovTalkStatusRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1"
           |}
           |""".stripMargin)

      val model = json.as[SelectGovTalkStatusRequest]
      model.userIdentifier mustBe "USER-1"
      model.formResultId mustBe "FR-1"

      Json.toJson(model).as[SelectGovTalkStatusRequest] mustBe model
    }
  }

  "SelectGovTalkFormResultIdRequest" should {
    "read and write fully populated" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1"
           |}
           |""".stripMargin)

      val model = json.as[SelectGovTalkFormResultIdRequest]
      model.userIdentifier mustBe "USER-1"

      Json.toJson(model).as[SelectGovTalkFormResultIdRequest] mustBe model
    }
  }

  "SelectGovTalkStatusResponse" should {
    "read and write fully populated with all optional fields present" in {
      val json = Json.parse(
        s"""
           |{
           |"userIdentifier":"USER-1",
           |"formResultId":"FR-1",
           |"correlationId":"CORR-1",
           |"formLock":"N",
           |"createTimestamp":"2026-01-02T09:00:00Z",
           |"endStateTimestamp":"2026-01-02T09:05:00Z",
           |"lastMessageTimestamp":"2026-01-02T09:04:00Z",
           |"numberOfPolls":"3",
           |"pollInterval":"10",
           |"protocolStatus":"SUBMITTED",
           |"gatewayUrl":"https://gateway.example/submit"
           |}
           |""".stripMargin)

      val model = json.as[SelectGovTalkStatusResponse]
      model.userIdentifier mustBe Some("USER-1")
      model.formResultId mustBe Some("FR-1")
      model.correlationId mustBe Some("CORR-1")
      model.formLock mustBe Some("N")
      model.createTimestamp mustBe Some("2026-01-02T09:00:00Z")
      model.endStateTimestamp mustBe Some("2026-01-02T09:05:00Z")
      model.lastMessageTimestamp mustBe Some("2026-01-02T09:04:00Z")
      model.numberOfPolls mustBe Some("3")
      model.pollInterval mustBe Some("10")
      model.protocolStatus mustBe Some("SUBMITTED")
      model.gatewayUrl mustBe Some("https://gateway.example/submit")

      Json.toJson(model).as[SelectGovTalkStatusResponse] mustBe model
    }

    "read and write with all optional fields absent" in {
      val json = Json.parse("{}")

      val model = json.as[SelectGovTalkStatusResponse]
      model.userIdentifier mustBe None
      model.formResultId mustBe None
      model.correlationId mustBe None
      model.formLock mustBe None
      model.createTimestamp mustBe None
      model.endStateTimestamp mustBe None
      model.lastMessageTimestamp mustBe None
      model.numberOfPolls mustBe None
      model.pollInterval mustBe None
      model.protocolStatus mustBe None
      model.gatewayUrl mustBe None

      // All-None serialises to {} (Option fields omitted on write).
      Json.toJson(model) mustBe Json.obj()
      Json.toJson(model).as[SelectGovTalkStatusResponse] mustBe model
    }
  }

  "SelectGovTalkFormResultIdResponse" should {
    "read and write with the optional field present" in {
      val json = Json.parse(
        s"""
           |{
           |"formResultId":"FR-1"
           |}
           |""".stripMargin)

      val model = json.as[SelectGovTalkFormResultIdResponse]
      model.formResultId mustBe Some("FR-1")

      Json.toJson(model).as[SelectGovTalkFormResultIdResponse] mustBe model
    }

    "read and write with the optional field absent" in {
      val json = Json.parse("{}")

      val model = json.as[SelectGovTalkFormResultIdResponse]
      model.formResultId mustBe None

      Json.toJson(model) mustBe Json.obj()
      Json.toJson(model).as[SelectGovTalkFormResultIdResponse] mustBe model
    }
  }

}