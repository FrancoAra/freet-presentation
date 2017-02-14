package models

import org.joda.time.LocalDate
import play.api.libs.json._

case class Booking(name: String, time: HHMM, day: LocalDate)

object Booking {

  implicit val jsonFormat: Format[Booking] = Json.format[Booking]
}
