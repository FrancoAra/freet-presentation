package models

import utils.algebra.Cats.TryAsJsResult
import utils.algebra.{ModuloN, ModuloNOf}
import cats.{Eq, Group, Order}
import play.api.libs.json._

import scala.util.Try

object Duration {

  val millisecond      = 1l
  val second: Long     = millisecond * 1000l
  val minute: Long     = second * 60l
  val hour: Long       = minute * 60l
  val maxSeconds: Long = second * 59l
  val maxMinutes: Long = minute * 59l
  val maxHours: Long   = hour * 23l
  val maxMMSS: Long    = maxMinutes + maxSeconds
  val maxHHMM: Long    = maxMinutes + maxHours
}

case class HHMM(repr: Int) {

  if (repr >= HHMM.limit)
    throw new IllegalArgumentException(s"Representation $repr is too large to be a proper HHMM")

  def duration: Long = {
    val ms = repr % 60
    val hs = repr / 60
    ms * Duration.minute + hs * Duration.hour
  }

  def string: String = {
    val hs      = (duration / 1000 / 60 / 60) % 24
    val ms      = (duration / 1000 / 60) % 60
    val hours   = if (hs < 10) s"0$hs" else s"$hs"
    val minutes = if (ms < 10) s"0$ms" else s"$ms"
    s"$hours:$minutes"
  }

  override def toString: String = s"HHMM($string)"
}

object HHMM {

  val limit: Int = 60 * 24

  val moduloN: ModuloN = ModuloN(limit)

  val regexp = """^(([0-1]\d)|(2[0-3])):([0-5]\d)$""".r

  implicit val constructor: Function[Long, HHMM] = apply

  def apply(duration: Long): HHMM = {
    val hs = ((duration / 1000 / 60 / 60) % 24).toInt
    val ms = ((duration / 1000 / 60)      % 60).toInt
    apply(hs * 60 + ms)
  }

  def apply(string: String): HHMM = {
    string match {
      case raw @ regexp(_ *) =>
        val split = raw.split(":")
        val hs    = split.head.toInt
        val ms    = split(1).toInt
        apply(hs * 60 + ms)
      case _ => throw new IllegalArgumentException(s"Invalid format (hh:mm) for '$string'")
    }
  }

  def parse(string: String): Try[HHMM] = Try(apply(string))

  implicit val jsonReads: Reads[HHMM] = Reads(_.validate[String].flatMap(x => TryAsJsResult(parse(x))))

  implicit val jsonWrites: Writes[HHMM] = Writes(x => JsString(x.string))

  implicit val moduleNOf: ModuloNOf[HHMM] = ModuloNOf[HHMM](moduloN)

  implicit val group: Group[HHMM] = new Group[HHMM] {
    override def empty: HHMM                     = apply(0)
    override def combine(x: HHMM, y: HHMM): HHMM = apply(moduloN.combine(x.repr, y.repr))
    override def inverse(a: HHMM): HHMM          = apply(moduloN.inverse(a.repr))
  }

  implicit val eq: Eq[HHMM] = new Eq[HHMM] {
    override def eqv(x: HHMM, y: HHMM): Boolean = x.repr == y.repr
  }

  implicit def order: Order[HHMM] = new Order[HHMM] {
    override def compare(x: HHMM, y: HHMM): Int = cats.instances.int.catsKernelStdOrderForInt.compare(x.repr, y.repr)
  }

  implicit def ordering: Ordering[HHMM] = order.toOrdering
}
