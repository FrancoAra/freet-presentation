package api

import cats.data.EitherT
import cats.{Id, Monad}
import models.store.StoreError
import play.api.Logger
import play.api.mvc.{Request, Result}
import services.booking.{BookingStore, BookingStoreError}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object HTTP extends APIResults {

  type Task[A] = EitherT[Id, Result, A]

  val unmatchedError: PartialFunction[Error, Result] = {
    case error =>
      Logger.error(s"Unmatched OrientTaskError, responding with Internal Server Error (500): $error")
      JsonInternalServerError("Unknown internal server error", error.toString)
  }

  def ok[A](value: A): Task[A] = EitherT[Id, Result, A](Right(value))

  def fail[A](result: Result): Task[A] = EitherT[Id, Result, A](Left(result))

  def find[A](option: Option[A], error: => Result): Task[A] =
    option match {
      case Some(entity) => ok(entity)
      case None => fail(error)
    }

  def bookingStore[A](task: BookingStore.Task[A]): Task[A] =
    BookingStore.run(task).leftMap(StoreError.http orElse BookingStoreError.http orElse unmatchedError)

  def fromTry[A](t: Try[A], err: Throwable => Result): Task[A] =
    t match {
      case Success(v) => ok(v)
      case Failure(NonFatal(e)) => fail(err(e))
    }

  def run(result: Task[Result]): Result =
    result.fold(identity, identity)
}
