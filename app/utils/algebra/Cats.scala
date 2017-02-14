package utils.algebra

import cats.~>
import play.api.data.validation._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

object Cats {

  object TryAsJsResult extends (Try ~> JsResult) {

    def apply[A](fa: Try[A]): JsResult[A] = fa match {
      case Success(a) => JsSuccess(a)
      case Failure(e) => JsError(__, e.getMessage)
    }
  }

  def TryAsPlayValidation[A](fa: Try[A]): ValidationResult = {
    fa match {
      case Success(_) => Valid
      case Failure(e) => Invalid(Seq(ValidationError(e.getMessage)))
    }
  }
}
