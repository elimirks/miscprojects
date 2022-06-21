package threadway

import java.io.BufferedWriter
import java.lang.StringBuilder
import scala.collection.mutable
import scala.io.Source
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.blocking
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.util.Success

object Main extends App {
  def updateWorld(input: Source): Unit = {
    /*
    implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
     */
    implicit val ec: ExecutionContext = ExecutionContext.global

    val things = for (i <- 0 until 10) yield Future {
      println(s"i: $i")
      Thread.sleep((i / 2) * 1000)
      i
    }

    things.foreach(f => println(Await.result(f, Duration.Inf)))

    val lines = input.getLines()
    val first = lines.nextOption.get

    // Feeling ringy
    var below  = first.map(_ == 'x').toArray
    var above  = Array.ofDim[Boolean](below.size)
    var middle = Array.ofDim[Boolean](below.size)

    var x = 0

    val processedRows = for {
      line <- lines
    } yield {
      above  = middle
      middle = below
      below  = line.map(_ == 'x').toArray
      x += 1000

      Future {
        println(x)
        Thread.sleep(x)
        processRow(above, middle, below) + s" from $line"
      }
    }

    processedRows.foreach(f => println(Await.result(f, Duration.Inf)))

    above  = middle
    middle = below
    below  = Array.ofDim[Boolean](below.size)
    println(processRow(above, middle, below))
  }

  // Requires the above and below rows to know the neighbor counts
  def processRow(
    above: Array[Boolean],
    middle: Array[Boolean],
    below: Array[Boolean],
  ): String = {
    val sb = new StringBuilder(middle.size)
    for (x <- 0 until middle.size) {
      val neighbors = neighborCount(above, middle, below, x)
      val alive = neighbors == 3 || (neighbors == 2 && middle(x))
      sb.append(if (alive) 'x' else '-')
    }
    sb.toString
  }

  @inline
  def neighborCount(
    above: Array[Boolean],
    middle: Array[Boolean],
    below: Array[Boolean],
    x: Int
  ): Int = {
    val minX = math.max(0, x - 1)
    val maxX = math.min(middle.size - 1, x + 1)
    var acc = 0
    for (xp <- minX to maxX) {
      if (xp != x && middle(xp)) acc += 1
      if (above(xp)) acc += 1
      if (below(xp)) acc += 1
    }
    acc
  }
}
