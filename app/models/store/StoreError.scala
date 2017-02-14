package models.store

import play.api.mvc.Result

trait StoreError extends Error
case class StoreNotFound(key: String) extends StoreError
case object StoreUnreachable extends StoreError

object StoreError extends api.APIResults {

  val http: PartialFunction[Error, Result] = {
    case StoreNotFound(key) => JsonNotFound(s"Entity with id $key not found.")
    case StoreUnreachable => JsonInternalServerError("Can't reach the database.")
  }
}
