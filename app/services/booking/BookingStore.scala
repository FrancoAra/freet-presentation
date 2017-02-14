package services.booking

import cats.{Id, ~>}
import models.store.InMemoryStore
import models.Booking
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Json

import scala.language.higherKinds

case class BookingWithKey(key: String, booking: Booking)

object BookingWithKey {

  implicit val format = Json.format[BookingWithKey]
}

object BookingStore extends InMemoryStore[Booking] {

  def book(ref: Booking): Task[BookingWithKey] = {
    val key = mkKey(ref)
    for {
      opt <- search(key)
      _ <- opt match {
        case Some(_) => fail(Booked(ref))
        case None => put(key, ref)
      }
    } yield BookingWithKey(key, ref)
  }

  def mkKey(booking: Booking): String = {
    val format = DateTimeFormat.forPattern("ddMMyyyy")
    val rawDay = booking.day.toString(format)
    val rawTime = booking.time.string.replace(":", "")
    rawDay + rawTime
  }
}




