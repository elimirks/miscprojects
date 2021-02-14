package scatto

import atto._
import Atto._
import cats.implicits._

object Main extends App {
  val sample: String = "Mo-Fr 08:00-12:00,13:00-17:30;Sa-Su 10:00-14:00"
  pprint.pprintln(AttoOSMParser.parse(sample))
  pprint.pprintln(CatsOSMParser.parse(sample))
}
