package utils

import algebra.Cats.TryAsJsResult
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json._

import scala.util.Try

object LocalDateHelpers {

  def parse(str: String): Try[LocalDate] =
    dateFormat1(str) orElse dateFormat2(str) orElse dateFormat3(str) orElse dateFormat4(str) orElse dateFormat5(str) orElse dateFormat6(
      str)

  implicit val jsonReads: Reads[LocalDate] = Reads(_.validate[String].flatMap(x => TryAsJsResult(parse(x))))

  implicit val jsonWrites: Writes[LocalDate] =
    Writes { date =>
      val format = DateTimeFormat.forPattern("dd-MM-yyyy")
      val string = date.toString(format)
      JsString(string)
    }

  implicit val orderingLocalDate: Ordering[LocalDate] =
    Ordering.by[LocalDate, (Int, Int, Int)](x => (x.getYear, x.getMonthOfYear, x.getDayOfMonth))

  private def dateFormat1(str: String): Try[LocalDate] =
    Try(LocalDate.parse(str, DateTimeFormat.forPattern("dd-mm-yyyy")))

  private def dateFormat2(str: String): Try[LocalDate] =
    Try(LocalDate.parse(str, DateTimeFormat.forPattern("dd/mm/yyyy")))

  private def dateFormat3(str: String): Try[LocalDate] =
    Try(LocalDate.parse(str, DateTimeFormat.forPattern("dd.mm.yyyy")))

  private def dateFormat4(str: String): Try[LocalDate] =
    Try(LocalDate.parse(str, DateTimeFormat.forPattern("yyyy-mm-dd")))

  private def dateFormat5(str: String): Try[LocalDate] =
    Try(LocalDate.parse(str, DateTimeFormat.forPattern("yyyy/mm/dd")))

  private def dateFormat6(str: String): Try[LocalDate] =
    Try(LocalDate.parse(str, DateTimeFormat.forPattern("yyyy.mm.dd")))
}
