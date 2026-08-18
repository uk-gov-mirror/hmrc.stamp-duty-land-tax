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

package connectors

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import com.github.tomakehurst.wiremock.http.Fault
import models.filing.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.HeaderCarrier

import scala.xml.Elem

class ChrisConnectorISpec
  extends AnyWordSpec
    with Matchers
    with ScalaFutures
    with IntegrationPatience
    with BeforeAndAfterAll:

  private val wireMockPort   = 11111
  private val wireMockServer = new WireMockServer(wireMockConfig().port(wireMockPort))


  override def beforeAll(): Unit =
    super.beforeAll()
    wireMockServer.start()
    WireMock.configureFor("localhost", wireMockPort)

  override def afterAll(): Unit =
    wireMockServer.stop()
    super.afterAll()

  private val app: Application =
    new GuiceApplicationBuilder()
      .configure(
        "microservice.services.chris.host" -> "localhost",
        "microservice.services.chris.port" -> wireMockPort,
        "microservice.services.chris.submit-url" -> "/ChRIS/SDLT/Filing/sync/SDLT"
      )
      .build()

  private lazy val connector: ChrisConnector = app.injector.instanceOf[ChrisConnector]

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val corrId    = "46DCD4CC7E194088B99857931C185829"
  private val envelope: Elem = <IRenvelope>test</IRenvelope>

  private val submitPath = "/ChRIS/SDLT/Filing/sync/SDLT"

  private def stubChris(status: Int, body: String): Unit =
    wireMockServer.stubFor(
      post(urlEqualTo(submitPath))
        .willReturn(aResponse().withStatus(status).withBody(body))
    )

  private def ackBody(pollInterval: String = "10"): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>acknowledgement</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |    <ResponseEndPoint PollInterval="$pollInterval"></ResponseEndPoint>
       |  </MessageDetails></Header>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin


  private def ackBodyNoPoll(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>acknowledgement</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |    <ResponseEndPoint></ResponseEndPoint>
       |  </MessageDetails></Header>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin

  private def ackBodyBadPoll(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>acknowledgement</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |    <ResponseEndPoint PollInterval="not-a-number"></ResponseEndPoint>
       |  </MessageDetails></Header>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin

  private def responseBody(utrn: String = "123456789MA", irMark: String = "RECEIVED-IRMARK"): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>response</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body>
       |    <SuccessResponse>
       |      <IRmark>$irMark</IRmark>
       |      <UTRN>$utrn</UTRN>
       |    </SuccessResponse>
       |  </Body>
       |</GovTalkMessage>""".stripMargin

  private def realChrisSuccessBody(utrn: String = "123456789MA", digest: String = "DkzkXJo7oa80QT6r9xfwXdArYFQ="): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Class>IR-SDLT-LTR</Class>
       |    <Qualifier>response</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body>
       |    <SuccessResponse xmlns="http://www.inlandrevenue.gov.uk/SuccessResponse">
       |      <IRmarkReceipt>
       |        <dsig:Signature xmlns:dsig="http://www.w3.org/2000/09/xmldsig#">
       |          <dsig:SignedInfo>
       |            <dsig:Reference>
       |              <dsig:DigestValue>$digest</dsig:DigestValue>
       |            </dsig:Reference>
       |          </dsig:SignedInfo>
       |        </dsig:Signature>
       |      </IRmarkReceipt>
       |      <AcceptedTime>2026-08-18T11:53:41.437</AcceptedTime>
       |      <UTRN>$utrn</UTRN>
       |    </SuccessResponse>
       |  </Body>
       |</GovTalkMessage>""".stripMargin

  private def responseNoDetailsBody(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>response</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body><SuccessResponse/></Body>
       |</GovTalkMessage>""".stripMargin


  private def responseCapitalIrMarkBody(irMark: String = "CAPITAL-IRMARK",
                                        utrn: String = "123456789MA"): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>response</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body>
       |    <SuccessResponse>
       |      <IRMark>$irMark</IRMark>
       |      <UTRN>$utrn</UTRN>
       |    </SuccessResponse>
       |  </Body>
       |</GovTalkMessage>""".stripMargin


  private def unexpectedQualifierBody(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>poll</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin

  private def errorBody(number: String, raisedBy: String, errorType: String,
                        includeBodyError: Boolean = false): String =
    val bodyErr =
      if includeBodyError then
        """<Body>
          |  <ErrorResponse SchemaVersion="2.0">
          |    <Error>
          |      <RaisedBy>System</RaisedBy>
          |      <Number>5005</Number>
          |      <Type>business</Type>
          |      <Text>Keys do not match.</Text>
          |      <Location>/IRenvelope[1]/IRheader[1]/Keys[1]/Key[1]</Location>
          |    </Error>
          |  </ErrorResponse>
          |</Body>""".stripMargin
      else "<Body/>"

    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>error</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <GovTalkDetails>
       |    <GovTalkErrors>
       |      <Error>
       |        <RaisedBy>$raisedBy</RaisedBy>
       |        <Number>$number</Number>
       |        <Type>$errorType</Type>
       |        <Text>error $number</Text>
       |      </Error>
       |    </GovTalkErrors>
       |  </GovTalkDetails>
       |  $bodyErr
       |</GovTalkMessage>""".stripMargin


  private def errorMultiTextBody(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>error</Qualifier>
       |    <Function>submit</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <GovTalkDetails>
       |    <GovTalkErrors>
       |      <Error>
       |        <RaisedBy>Department</RaisedBy>
       |        <Number>3001</Number>
       |        <Type>business</Type>
       |        <Text>First line.</Text>
       |        <Text>Second line.</Text>
       |      </Error>
       |    </GovTalkErrors>
       |  </GovTalkDetails>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin

  private def deleteResponseBody(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>response</Qualifier>
       |    <Function>delete</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin

  private def deleteErrorBody(number: String): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>error</Qualifier>
       |    <Function>delete</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <GovTalkDetails>
       |    <GovTalkErrors>
       |      <Error>
       |        <RaisedBy>Gateway</RaisedBy>
       |        <Number>$number</Number>
       |        <Type>fatal</Type>
       |        <Text>delete error $number</Text>
       |      </Error>
       |    </GovTalkErrors>
       |  </GovTalkDetails>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin

  private def deleteUnexpectedQualifierBody(): String =
    s"""<?xml version="1.0"?>
       |<GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
       |  <EnvelopeVersion>2.0</EnvelopeVersion>
       |  <Header><MessageDetails>
       |    <Qualifier>acknowledgement</Qualifier>
       |    <Function>delete</Function>
       |    <CorrelationID>$corrId</CorrelationID>
       |  </MessageDetails></Header>
       |  <Body/>
       |</GovTalkMessage>""".stripMargin


  "ChrisConnector.submit" should {

    "parse a SUBMISSION_RESPONSE into Completed with UTRN and received IRmark" in {
      stubChris(200, responseBody(utrn = "123456789MA", irMark = "RECEIVED-IRMARK"))

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed =>
          c.utrn shouldBe Some("123456789MA")
          c.receivedIrMark shouldBe Some("RECEIVED-IRMARK")
          c.correlationId shouldBe Some(corrId)
        case other => fail(s"expected Completed, got $other")
    }

    "parse a SUBMISSION_ACKNOWLEDGEMENT into Acknowledged with the poll interval" in {
      stubChris(200, ackBody(pollInterval = "15"))

      connector.submit(envelope, None, corrId).futureValue match
        case a: ChrisResponse.Acknowledged =>
          a.correlationId shouldBe Some(corrId)
          a.pollIntervalSeconds shouldBe Some(15)
        case other => fail(s"expected Acknowledged, got $other")
    }

    "parse a departmental 3001 error into Errored, collecting header and body errors" in {
      stubChris(200, errorBody("3001", raisedBy = "Department", errorType = "business", includeBodyError = true))

      connector.submit(envelope, None, corrId).futureValue match
        case e: ChrisResponse.Errored =>
          e.isBusinessReject shouldBe true
          e.errors.map(_.number) should contain allOf (Some("3001"), Some("5005"))
          e.fieldErrors.exists(_.location.isDefined) shouldBe true
        case other => fail(s"expected Errored, got $other")
    }

    "parse a gateway 1001 error into Errored (not a business reject)" in {
      stubChris(200, errorBody("1001", raisedBy = "Gateway", errorType = "fatal"))

      connector.submit(envelope, None, corrId).futureValue match
        case e: ChrisResponse.Errored =>
          e.isBusinessReject shouldBe false
          e.errors.map(_.number) should contain (Some("1001"))
        case other => fail(s"expected Errored, got $other")
    }

    "return TransportError for a permanent non-2xx response (404)" in {
      stubChris(404, "not found")

      connector.submit(envelope, None, corrId).futureValue match
        case ChrisResponse.TransportError(msg, _) => msg should include ("404")
        case other => fail(s"expected TransportError, got $other")
    }

    "return a retryable Errored (code 2005) for a transient 503 response" in {
      stubChris(503, "service unavailable")

      connector.submit(envelope, None, corrId).futureValue match
        case e: ChrisResponse.Errored => e.errors.map(_.number) should contain (Some("2005"))
        case other => fail(s"expected a retryable Errored, got $other")
    }

    "treat a transient 503 as recoverable" in {
      stubChris(503, "service unavailable")

      val resp = connector.submit(envelope, None, corrId).futureValue
      UniversalStatus.fromChrisResponse(resp, Some("IRMARK")) shouldBe UniversalStatus.STARTED
    }

    "treat transient statuses 408, 429, 500, 502, 503 and 504 as retryable Errored (2005)" in {
      Seq(408, 429, 500, 502, 503, 504).foreach { s =>
        stubChris(s, s"transient $s")
        connector.submit(envelope, None, corrId).futureValue match
          case e: ChrisResponse.Errored => e.errors.map(_.number) should contain (Some("2005"))
          case other => fail(s"expected a retryable Errored for status $s, got $other")
      }
    }

    "return TransportError for an unparseable body" in {
      stubChris(200, "not xml at all")

      connector.submit(envelope, None, corrId).futureValue shouldBe a [ChrisResponse.TransportError]
    }

    "POST the envelope with the CorrelationId header to the chris endpoint" in {
      stubChris(200, ackBody())

      connector.submit(envelope, None, corrId).futureValue

      wireMockServer.verify(
        postRequestedFor(urlEqualTo(submitPath))
          .withHeader("CorrelationId", equalTo(corrId))
          .withHeader("Content-Type", containing("xml"))
      )
    }

    "parse a SUBMISSION_RESPONSE with neither UTRN nor IRmark into Completed(None, None)" in {
      stubChris(200, responseNoDetailsBody())

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed =>
          c.utrn shouldBe None
          c.receivedIrMark shouldBe None
          c.correlationId shouldBe Some(corrId)
        case other => fail(s"expected Completed, got $other")
    }

    "ignore a malformed UTRN element and surface no UTRN" in {
      stubChris(200, responseBody(utrn = "NOT-A-UTRN", irMark = "RECEIVED-IRMARK"))

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed =>
          c.utrn shouldBe None
          c.receivedIrMark shouldBe Some("RECEIVED-IRMARK")
        case other => fail(s"expected Completed, got $other")
    }

    "read the IRmark from the alternative IRMark spelling" in {
      stubChris(200, responseCapitalIrMarkBody(irMark = "CAPITAL-IRMARK"))

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed => c.receivedIrMark shouldBe Some("CAPITAL-IRMARK")
        case other => fail(s"expected Completed, got $other")
    }

    "read the IRmark from the IRmarkReceipt digest" in {
      stubChris(200, realChrisSuccessBody(digest = "DkzkXJo7oa80QT6r9xfwXdArYFQ="))

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed => c.receivedIrMark shouldBe Some("DkzkXJo7oa80QT6r9xfwXdArYFQ=")
        case other => fail(s"expected Completed, got $other")
    }

    "resolve a return to SUBMITTED when the digest in a real ChRIS response matches the mark we sent" in {
      stubChris(200, realChrisSuccessBody(digest = "SENT-MARK"))

      val resp = connector.submit(envelope, None, corrId).futureValue
      UniversalStatus.fromChrisResponse(resp, Some("SENT-MARK")) shouldBe UniversalStatus.SUBMITTED
    }

    "reformat the accepted time from a real ChRIS response for FormP" in {
      stubChris(200, realChrisSuccessBody())

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed => c.acceptedTime shouldBe Some("2026-08-18 11:53:41.437")
        case other => fail(s"expected Completed, got $other")
    }

    "leave the accepted time empty when the response does not carry one" in {
      stubChris(200, responseCapitalIrMarkBody(irMark = "CAPITAL-IRMARK"))

      connector.submit(envelope, None, corrId).futureValue match
        case c: ChrisResponse.Completed => c.acceptedTime shouldBe None
        case other => fail(s"expected Completed, got $other")
    }

    "parse an acknowledgement with no PollInterval into Acknowledged(None)" in {
      stubChris(200, ackBodyNoPoll())

      connector.submit(envelope, None, corrId).futureValue match
        case a: ChrisResponse.Acknowledged => a.pollIntervalSeconds shouldBe None
        case other => fail(s"expected Acknowledged, got $other")
    }

    "parse an acknowledgement with a non-numeric PollInterval into Acknowledged(None)" in {
      stubChris(200, ackBodyBadPoll())

      connector.submit(envelope, None, corrId).futureValue match
        case a: ChrisResponse.Acknowledged => a.pollIntervalSeconds shouldBe None
        case other => fail(s"expected Acknowledged, got $other")
    }

    "collapse multiple Error Text lines into a single space-joined message" in {
      stubChris(200, errorMultiTextBody())

      connector.submit(envelope, None, corrId).futureValue match
        case e: ChrisResponse.Errored =>
          e.errors.headOption.flatMap(_.text) shouldBe Some("First line. Second line.")
        case other => fail(s"expected Errored, got $other")
    }

    "return TransportError for an unexpected GovTalk qualifier/function" in {
      stubChris(200, unexpectedQualifierBody())

      connector.submit(envelope, None, corrId).futureValue match
        case ChrisResponse.TransportError(msg, _) => msg should include ("Unexpected GovTalk message")
        case other => fail(s"expected TransportError, got $other")
    }

    "return an Errored carrying the transport failure message when the connection is reset mid-request" in {
      wireMockServer.stubFor(
        post(urlEqualTo(submitPath))
          .willReturn(aResponse().withFault(Fault.CONNECTION_RESET_BY_PEER))
      )

      connector.submit(envelope, None, corrId).futureValue match
        case e: ChrisResponse.Errored =>
          e.correlationId shouldBe Some(corrId)
          e.errors.headOption.flatMap(_.text) should not be empty
        case other => fail(s"expected Errored, got $other")
    }

    "POST to the supplied endpoint when one is given, not the default path" in {
      val overridePath = "/override-endpoint"
      wireMockServer.stubFor(
        post(urlEqualTo(overridePath))
          .willReturn(aResponse().withStatus(200).withBody(ackBody()))
      )

      connector.submit(envelope, Some(s"http://localhost:$wireMockPort$overridePath"), corrId).futureValue

      wireMockServer.verify(postRequestedFor(urlEqualTo(overridePath)))
    }
  }

  "ChrisConnector.delete" should {

    "parse a DELETE_RESPONSE into Deleted" in {
      stubChris(200, deleteResponseBody())

      connector.delete(None, corrId).futureValue match
        case d: ChrisDeleteResponse.Deleted => d.correlationId shouldBe Some(corrId)
        case other => fail(s"expected Deleted, got $other")
    }

    "map a 2000 error to NotFound (resource already gone)" in {
      stubChris(200, deleteErrorBody("2000"))

      connector.delete(None, corrId).futureValue match
        case n: ChrisDeleteResponse.NotFound => n.correlationId shouldBe Some(corrId)
        case other => fail(s"expected NotFound, got $other")
    }

    "parse a non-2000 error into Errored" in {
      stubChris(200, deleteErrorBody("1035"))

      connector.delete(None, corrId).futureValue match
        case e: ChrisDeleteResponse.Errored =>
          e.errors.map(_.number) should contain(Some("1035"))
        case other => fail(s"expected Errored, got $other")
    }

    "return TransportError for an unexpected GovTalk qualifier/function" in {
      stubChris(200, deleteUnexpectedQualifierBody())

      connector.delete(None, corrId).futureValue match
        case ChrisDeleteResponse.TransportError(msg, _) => msg should include("Unexpected GovTalk message")
        case other => fail(s"expected TransportError, got $other")
    }

    "return TransportError for a non-2xx response" in {
      stubChris(500, "boom")

      connector.delete(None, corrId).futureValue match
        case ChrisDeleteResponse.TransportError(msg, _) => msg should include("500")
        case other => fail(s"expected TransportError, got $other")
    }

    "return TransportError for an unparseable body" in {
      stubChris(200, "not xml at all")

      connector.delete(None, corrId).futureValue shouldBe a[ChrisDeleteResponse.TransportError]
    }

    "return TransportError when the connection is reset mid-request" in {
      wireMockServer.stubFor(
        post(urlEqualTo(submitPath))
          .willReturn(aResponse().withFault(Fault.CONNECTION_RESET_BY_PEER))
      )

      connector.delete(None, corrId).futureValue shouldBe a[ChrisDeleteResponse.TransportError]
    }

    "POST the delete envelope with the CorrelationId header to the default path" in {
      stubChris(200, deleteResponseBody())

      connector.delete(None, corrId).futureValue

      wireMockServer.verify(
        postRequestedFor(urlEqualTo(submitPath))
          .withHeader("CorrelationId", equalTo(corrId))
          .withHeader("Content-Type", containing("xml"))
          .withRequestBody(containing("<Function>delete</Function>"))
      )
    }

    "POST to the supplied endpoint when one is given, not the default path" in {
      val overridePath = "/delete-endpoint"
      wireMockServer.stubFor(
        post(urlEqualTo(overridePath))
          .willReturn(aResponse().withStatus(200).withBody(deleteResponseBody()))
      )

      connector.delete(Some(s"http://localhost:$wireMockPort$overridePath"), corrId).futureValue

      wireMockServer.verify(postRequestedFor(urlEqualTo(overridePath)))
    }
  }