package api

import models.{Booking, HHMM}
import play.api.mvc._
import play.api.libs.json._
import utils.LocalDateHelpers
import HTTP._
import services.booking.BookingStore

class BookingEndpoint extends Controller with APIResults {

  def book(name: String, rawTime: String, rawDay: String) = Action { request =>
    run(for {
      time <- fromTry(HHMM.parse(rawTime), _ => JsonBadRequest(s"Invalid time $rawTime"))
      day <- fromTry(LocalDateHelpers.parse(rawDay), _ => JsonBadRequest(s"Invalid day $rawDay"))
      booking <- bookingStore(BookingStore.book(Booking(name, time, day)))
    } yield JsonOk(booking))
  }

  def get(key: String) = Action { request =>
    run(bookingStore(BookingStore.find(key).map(JsonOk(_))))
  }

  def delete(key: String) = Action { request =>
    run(bookingStore(BookingStore.delete(key).map(JsonOk(_))))
  }
}


