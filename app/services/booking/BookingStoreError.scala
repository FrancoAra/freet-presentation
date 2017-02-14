package services.booking

import models.Booking
import models.store.StoreError
import play.api.mvc.Result

trait BookingStoreError extends Error
case class Booked(ref: Booking) extends BookingStoreError

object BookingStoreError extends api.APIResults {

  val http: PartialFunction[Error, Result] = {
    case Booked(ref) => JsonOkUnsuccessful(s"Booking unavailable", ref)
  }
}

