package api

import cats.data.NonEmptyList
import play.api.libs.json._
import play.api.mvc.Results
import play.api.Logger

trait APIResults extends Results {

  def success(a: JsValue) = Json.obj("success" -> true, "data" -> a)

  def success[A](a: A)(implicit w: Writes[A]) = Json.obj("success" -> true, "data" -> a)

  def failure(message: String) = {
    val json = Json.obj("success" -> false, "message" -> message)
    Logger.debug(json.toString)
    json
  }

  def failure(messages: Seq[String]) = {
    val json = Json.obj("success" -> false, "messages" -> messages)
    Logger.debug(json.toString)
    json
  }

  def failure[A](message: String, data: A)(implicit writes: Writes[A]) = {
    val json = Json.obj("success" -> false, "message" -> message, "data" -> writes.writes(data))
    Logger.debug(json.toString)
    json
  }

  def failure[E](errors: NonEmptyList[E])(implicit w: Writes[E]) =
    Json.obj("success" -> false, "errors" -> errors.toList)

  def JsonOk = Ok(success(JsNull))

  def JsonOk(json: JsValue) = Ok(success(json))

  def JsonOk[A](a: A)(implicit w: Writes[A]) = Ok(success(a))

  def JsonOkUnsuccessful(message: String) = Ok(failure(message))

  def JsonOkUnsuccessful[A](message: String, a: A)(implicit w: Writes[A]) = Ok(failure(message, a))

  def JsonFailure(message: String) = Ok(failure(message))

  def JsonFailure(messages: Seq[String]) = Ok(failure(messages))

  def JsonNotFound(message: String) = NotFound(failure(message))

  def JsonNotFound(messages: Seq[String]) = NotFound(failure(messages))

  def JsonUnauthorized(message: String) = Unauthorized(failure(message))

  def JsonUnauthorized(messages: Seq[String]) = Unauthorized(failure(messages))

  def JsonBadRequest(message: String) = BadRequest(failure(message))

  def JsonBadRequest(messages: Seq[String]) = BadRequest(failure(messages))

  def JsonBadRequest[A](message: String, data: A)(implicit w: Writes[A]) = BadRequest(failure(message, data))

  def JsonInternalServerError(message: String) = InternalServerError(failure(message))

  def JsonInternalServerError(messages: Seq[String]) = InternalServerError(failure(messages))

  def JsonInternalServerError[A](message: String, data: A)(implicit w: Writes[A]) =
    InternalServerError(failure(message, data))

  def JsonBadRequest[E](errors: NonEmptyList[E])(implicit w: Writes[E]) = BadRequest(failure(errors))
}

case class JsonValidationError(message: String, path: String = "")

object JsonValidationError {
  implicit val jsonFormat: Format[JsonValidationError] = Json.format[JsonValidationError]
}
