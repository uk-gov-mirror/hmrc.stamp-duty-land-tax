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

import com.google.inject.Inject
import play.api.Logging
import play.api.libs.ws.DefaultBodyWritables.writeableOf_String
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import models.filing.*
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.xml.{Elem, Node, XML}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import uk.gov.hmrc.http.HttpReads.Implicits.*

class ChrisConnector @Inject() (
                                 httpClient: HttpClientV2,
                                 appConfig: ServicesConfig
                               )(implicit ec: ExecutionContext)
  extends Logging:

  private val chrisUrl: String   = appConfig.baseUrl("chris")
  private val submitPath: String = chrisUrl + appConfig.getConfString("chris.submit-url", "/ChRIS/SDLT/Filing/sync/SDLT")
  val defaultPath: String = submitPath
  private val messageClass: String ="IR-SDLT-LTR"
  private val requestTimeout: Duration = 120.seconds
  private val UtrnPattern: Regex = "^[0-9]{9}M[A-HJ-NP-TV-Z]$".r
  private val XmlDecl: String = """<?xml version="1.0" encoding="UTF-8"?>"""
  private val stubMode: Boolean = appConfig.getConfBool("chris.stub", false)

  private val RetryableHttpStatuses: Set[Int] = Set(408, 429, 500, 502, 503, 504)

  private def withResourceRefKey(envelope: Elem, ref: String): Elem =
    val key = <Key Type="ReturnResourceRef">
      {ref}
    </Key>
    val rule = new RewriteRule:
      override def transform(n: Node): Seq[Node] = n match
        case e: Elem if e.label == "Keys" => e.copy(child = e.child ++ key)
        case other => other
    new RuleTransformer(rule).transform(envelope).collectFirst { case e: Elem => e }.getOrElse(envelope)

  def submit(envelope: scala.xml.Elem, endpoint: Option[String], correlationId: String,
             returnResourceRef: Option[String] = None)(implicit hc: HeaderCarrier): Future[ChrisResponse] =
    val target = endpoint.filter(_.nonEmpty).getOrElse(defaultPath)
    val toSend =
      if stubMode then returnResourceRef.map(_.trim).filter(_.nonEmpty).map(withResourceRefKey(envelope, _)).getOrElse(envelope)
      else envelope
    val xmlString = XmlDecl + "\n" + toSend.toString()
    logger.info(s"[ChrisConnector] SUBMIT target=$target corrId=$correlationId stubMode=$stubMode ref=${returnResourceRef.getOrElse("-")} xml=$xmlString")
    httpClient
      .post(url"$target")
      .setHeader("Content-Type" -> "application/xml", "Accept" -> "application/xml", "CorrelationId" -> correlationId)
      .withBody(xmlString)
      .transform(_.withRequestTimeout(requestTimeout))
      .execute[HttpResponse]
      .map { resp =>
        if is2xx(resp.status) then parse(resp.body)
        else if RetryableHttpStatuses.contains(resp.status) then
          logger.warn(s"[ChrisConnector] transient NON-2xx (retryable -> STARTED) corrId=$correlationId status=${resp.status} body:\n${resp.body}")
          ChrisResponse.Errored(Seq(retryableHttp(resp.status)), Some(correlationId), None, resp.body)
        else
          logger.error(s"[ChrisConnector] NON-2xx (fatal) corrId=$correlationId status=${resp.status} body:\n${resp.body}")
          ChrisResponse.TransportError(s"NON-2xx status=${resp.status}")
      }
      .recover {
        case e: java.util.concurrent.TimeoutException =>
          logger.error(s"[ChrisConnector] Timeout after ${requestTimeout} corrId=$correlationId", e)
          ChrisResponse.Errored(Seq(timeout2005), Some(correlationId), None, "<timeout/>")
        case NonFatal(e) if isTimeout(e) =>
          logger.error(s"[ChrisConnector] Timeout corrId=$correlationId", e)
          ChrisResponse.Errored(Seq(timeout2005), Some(correlationId), None, "<timeout/>")
        case NonFatal(e) =>
          logger.error(s"[ChrisConnector] Transport exception corrId=$correlationId", e)
          ChrisResponse.Errored(
            Seq(transportFatal(Option(e.getMessage).getOrElse(e.toString))),
            Some(correlationId),
            None,
            "<transport-error/>"
          )
      }

  def poll(endpoint: Option[String], correlationId: String)(implicit hc: HeaderCarrier): Future[ChrisResponse] =
    val target    = endpoint.filter(_.nonEmpty).getOrElse(defaultPath)
    val xmlString = XmlDecl + "\n" + pollEnvelope(correlationId).toString()
    logger.debug(s"[ChrisConnector] POLL target=$target corrId=$correlationId")
    httpClient
      .post(url"$target")
      .setHeader(
        "Content-Type" -> "application/xml",
        "Accept" -> "application/xml",
        "CorrelationId" -> correlationId
      )
      .withBody(xmlString)
      .transform(_.withRequestTimeout(requestTimeout))
      .execute[HttpResponse]
      .map { resp =>
        if is2xx(resp.status) then parse(resp.body)
        else if RetryableHttpStatuses.contains(resp.status) then
          logger.warn(s"[ChrisConnector] POLL non-2xx, retrying next cycle corrId=$correlationId status=${resp.status} body:\n${resp.body}")
          ChrisResponse.TransportError(s"transient HTTP ${resp.status}")
        else
          logger.error(s"[ChrisConnector] POLL NON-2xx corrId=$correlationId status=${resp.status} body:\n${resp.body}")
          ChrisResponse.TransportError(s"NON-2xx status=${resp.status}")
      }
      .recover {
        case e: java.util.concurrent.TimeoutException =>
          logger.warn(s"[ChrisConnector] POLL timeout after ${requestTimeout}, retrying next cycle corrId=$correlationId", e)
          ChrisResponse.TransportError("client timeout", "<timeout/>")
        case NonFatal(e) if isTimeout(e) =>
          logger.warn(s"[ChrisConnector] POLL timeout, retrying next cycle corrId=$correlationId", e)
          ChrisResponse.TransportError("client timeout", "<timeout/>")
        case NonFatal(e) =>
          logger.error(s"[ChrisConnector] POLL transport exception corrId=$correlationId", e)
          ChrisResponse.TransportError(Option(e.getMessage).getOrElse(e.toString))
      }

  private def pollEnvelope(correlationId: String): Elem =
    <GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
      <EnvelopeVersion>2.0</EnvelopeVersion>
      <Header>
        <MessageDetails>
          <Class>{messageClass}</Class>
          <Qualifier>poll</Qualifier>
          <Function>submit</Function>
          <CorrelationID>{correlationId}</CorrelationID>
          <Transformation>XML</Transformation>
        </MessageDetails>
        <SenderDetails/>
      </Header>
      <GovTalkDetails>
        <Keys/>
      </GovTalkDetails>
      <Body/>
    </GovTalkMessage>

  def delete(endpoint: Option[String], correlationId: String)(implicit hc: HeaderCarrier): Future[ChrisDeleteResponse] =
    val target    = endpoint.filter(_.nonEmpty).getOrElse(defaultPath)
    val xmlString = XmlDecl + "\n" + deleteEnvelope(correlationId).toString()
    logger.debug(s"[ChrisConnector] DELETE target=$target corrId=$correlationId")
    httpClient
      .post(url"$target")
      .setHeader(
        "Content-Type" -> "application/xml",
        "Accept" -> "application/xml",
        "CorrelationId" -> correlationId
      )
      .withBody(xmlString)
      .transform(_.withRequestTimeout(requestTimeout))
      .execute[HttpResponse]
      .map { resp =>
        if is2xx(resp.status) then parseDelete(resp.body)
        else
          logger.error(s"[ChrisConnector] DELETE NON-2xx corrId=$correlationId status=${resp.status} body:\n${resp.body}")
          ChrisDeleteResponse.TransportError(s"NON-2xx status=${resp.status}", resp.body)
      }
      .recover {
        case e: java.util.concurrent.TimeoutException =>
          logger.error(s"[ChrisConnector] DELETE timeout after ${requestTimeout} corrId=$correlationId", e)
          ChrisDeleteResponse.TransportError("client timeout", "<timeout/>")
        case NonFatal(e) if isTimeout(e) =>
          logger.error(s"[ChrisConnector] DELETE timeout corrId=$correlationId", e)
          ChrisDeleteResponse.TransportError("client timeout", "<timeout/>")
        case NonFatal(e) =>
          logger.error(s"[ChrisConnector] DELETE transport exception corrId=$correlationId", e)
          ChrisDeleteResponse.TransportError(Option(e.getMessage).getOrElse(e.toString))
      }

  private def deleteEnvelope(correlationId: String): Elem =
    <GovTalkMessage xmlns="http://www.govtalk.gov.uk/CM/envelope">
      <EnvelopeVersion>2.0</EnvelopeVersion>
      <Header>
        <MessageDetails>
          <Class>{messageClass}</Class>
          <Qualifier>request</Qualifier>
          <Function>delete</Function>
          <CorrelationID>{correlationId}</CorrelationID>
          <Transformation>XML</Transformation>
        </MessageDetails>
        <SenderDetails/>
      </Header>
      <GovTalkDetails>
        <Keys/>
      </GovTalkDetails>
      <Body/>
    </GovTalkMessage>

  private def parseDelete(body: String): ChrisDeleteResponse =
    try
      val xml       = XML.loadString(body)
      val qualifier = text(xml, "MessageDetails", "Qualifier").toLowerCase
      val function  = text(xml, "MessageDetails", "Function").toLowerCase
      val corrId    = Option(text(xml, "MessageDetails", "CorrelationID")).filter(_.nonEmpty)

      (qualifier, function) match
        case ("response", "delete") =>
          ChrisDeleteResponse.Deleted(corrId, body)

        case ("error", _) =>
          val errors = parseErrors(xml)
          if errors.exists(_.number.contains("2000")) then ChrisDeleteResponse.NotFound(corrId, body)
          else ChrisDeleteResponse.Errored(errors, corrId, body)

        case other =>
          logger.error(s"[ChrisConnector] Unexpected GovTalk qualifier/function for DELETE $other")
          ChrisDeleteResponse.TransportError(s"Unexpected GovTalk message: $other", body)
    catch
      case NonFatal(e) =>
        logger.error(s"[ChrisConnector] Failed to parse DELETE_RESPONSE", e)
        ChrisDeleteResponse.TransportError(s"Unparseable GovTalk response: ${e.getMessage}", body)

  private def is2xx(status: Int): Boolean = status >= 200 && status < 300

  private val timeout2005: GovTalkError =
    GovTalkError(
      raisedBy = "Gateway",
      number = Some("2005"),
      errorType = "timeOut",
      text = Some("The Service has not received an acknowledgement of your submission within the permitted timescale (client timeout)."),
      location = None
    )

  private def retryableHttp(status: Int): GovTalkError =
    GovTalkError(
      raisedBy = "Gateway",
      number = Some("2005"),
      errorType = "timeOut",
      text = Some(s"ChRIS returned a transient HTTP $status; the submission was not acknowledged and can be retried."),
      location = None
    )

  private def transportFatal(message: String): GovTalkError =
    GovTalkError(
      raisedBy = "Gateway",
      number = None,
      errorType = "fatal",
      text = Some(message),
      location = None
    )

  private def isTimeout(e: Throwable): Boolean =
    val m = Option(e.getMessage).getOrElse("").toLowerCase
    m.contains("timeout") || m.contains("timed out") || m.contains("request timeout")

  private def parse(body: String): ChrisResponse =
    try
      val xml = XML.loadString(body)
      val qualifier = text(xml, "MessageDetails", "Qualifier").toLowerCase
      val function = text(xml, "MessageDetails", "Function").toLowerCase
      val corrId = Option(text(xml, "MessageDetails", "CorrelationID")).filter(_.nonEmpty)

      (qualifier, function) match
        case ("response", "submit") =>
          logger.info("[ChrisConnector][parse] response submit" + body + xml)
          val irMark = extractIrMark(xml)
          val accepted = extractAcceptedTime(xml)
          if irMark.isEmpty then
            logger.warn(s"[ChrisConnector] no IRmark in response corrId=${corrId.getOrElse("-")}")
          else
            logger.info(s"[ChrisConnector] IRmark source=${irMarkSource(xml)} corrId=${corrId.getOrElse("-")}")
          if accepted.isEmpty then
            logger.info(s"[ChrisConnector] no AcceptedTime in response, using current time corrId=${corrId.getOrElse("-")}")
          ChrisResponse.Completed(extractUtrn(xml), irMark, corrId, responseEndPoint(xml), body, accepted)

        case ("error", _) =>
          logger.info("[ChrisConnector][parse] response error" + body + xml)
          ChrisResponse.Errored(parseErrors(xml), corrId, responseEndPoint(xml), body)

        case ("acknowledgement", _) =>
          logger.info("[ChrisConnector][parse] response acknowledgement" + body + xml)
          ChrisResponse.Acknowledged(corrId, pollInterval(xml), responseEndPoint(xml), body, extractAcceptedTime(xml))

        case other =>
          logger.error(s"[ChrisConnector] Unexpected GovTalk qualifier/function $other" + xml)
          ChrisResponse.TransportError(s"Unexpected GovTalk message: $other")
    catch
      case NonFatal(e) =>
        logger.error(s"[ChrisConnector] Failed to parse GovTalk response", e)
        ChrisResponse.TransportError(s"Unparseable GovTalk response: ${e.getMessage}")

  private def parseErrors(xml: Node): Seq[GovTalkError] =
    val headerErrors = (xml \\ "GovTalkErrors" \ "Error").map(toGovTalkError)
    val bodyErrors   = (xml \\ "ErrorResponse" \ "Error").map(toGovTalkError)
    headerErrors ++ bodyErrors

  private def toGovTalkError(e: Node): GovTalkError =
    GovTalkError(
      raisedBy  = (e \ "RaisedBy").text.trim,
      number    = Option((e \ "Number").text.trim).filter(_.nonEmpty),
      errorType = (e \ "Type").text.trim,
      text      = Some((e \ "Text").map(_.text.trim).filter(_.nonEmpty).mkString(" ")).filter(_.nonEmpty),
      location  = Option((e \ "Location").text.trim).filter(_.nonEmpty)
    )

  private def pollInterval(xml: Node): Option[Int] =
    (xml \\ "ResponseEndPoint" \ "@PollInterval").headOption
      .map(_.text.trim)
      .flatMap(s => scala.util.Try(s.toInt).toOption)

  private def responseEndPoint(xml: Node): Option[String] =
    (xml \\ "ResponseEndPoint").headOption.map(_.text.trim).filter(_.nonEmpty)

  private def extractUtrn(responseXml: Node): Option[String] =
    val explicit = (responseXml \\ "UTRN").map(_.text.trim).find(UtrnPattern.matches)
    explicit.orElse {
      UtrnPattern.findFirstIn((responseXml \\ "_").map(_.text).mkString(" "))
    }

  private val AcceptedTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  private def extractAcceptedTime(responseXml: Node): Option[String] =
    (responseXml \\ "AcceptedTime").map(_.text.trim).find(_.nonEmpty)
      .flatMap(raw => Try(LocalDateTime.parse(raw, DateTimeFormatter.ISO_LOCAL_DATE_TIME)).toOption)
      .map(_.format(AcceptedTimeFormatter))

  private def irMarkSource(responseXml: Node): String =
    if (responseXml \\ "IRmarkReceipt" \\ "DigestValue").exists(_.text.trim.nonEmpty) then "IRmarkReceipt/DigestValue"
    else if (responseXml \\ "IRmark").exists(_.text.trim.nonEmpty) then "IRmark"
    else if (responseXml \\ "IRMark").exists(_.text.trim.nonEmpty) then "IRMark"
    else "none"

  private def extractIrMark(responseXml: Node): Option[String] =
    (responseXml \\ "IRmarkReceipt" \\ "DigestValue").map(_.text.trim).find(_.nonEmpty)
      .orElse((responseXml \\ "IRmark").map(_.text.trim).find(_.nonEmpty))
      .orElse((responseXml \\ "IRMark").map(_.text.trim).find(_.nonEmpty))

  private def text(xml: Node, parent: String, child: String): String =
    (xml \\ parent \ child).headOption.map(_.text.trim).getOrElse("")