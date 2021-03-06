import cats.implicits._

import Helper.parsePrint
import OSMParser.recordsP

object Main extends App {
  parsePrint(recordsP, "Mo-Fr 08:00-12:00,13:00-17:30; Sa-Su 10:00 - 14:00")
}
