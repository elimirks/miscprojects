package scalaparsing

import cats.implicits._
import pprint.pprintln

object Main extends App {
  val sample: String = "Mo-Fr 08:00-12:00,13:00-17:30; Sa-Su 10:00 - 14:00"
  pprintln(OSMParser.parse(sample))
}
