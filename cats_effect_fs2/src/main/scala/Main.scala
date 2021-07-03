import cats.effect.{Concurrent, IO, IOApp, LiftIO}
import cats.effect.IO.asyncForIO
import cats.syntax.all._
import com.comcast.ip4s._
import fs2.{Chunk, Stream, Pipe, text}
import fs2.io.net.{Socket, Network}

import fs2.Stream
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration._

object HelloWorld extends IOApp.Simple {
  def run: IO[Unit] = program.compile.drain

  def echoServer(
    terminator: Deferred[IO, Unit]
  ): Stream[IO, Unit] =
    Network[IO].server(port = Some(port"6000")).map(client => {
      for {
        _ <- Stream.eval(client.write(Chunk.array("hello\n".getBytes())))
        _ <- client.reads
          .through(text.utf8Decode)
          .through(handle(terminator))
          .through(text.utf8Encode)
          .through(client.writes)
          .handleErrorWith(_ => Stream.empty) // handle errors of client sockets
      } yield ()
    }).parJoin(100)

  def handle(
    terminator: Deferred[IO, Unit]
  )(
    a: Stream[IO, String]
  ): Stream[IO, String] = {
    for {
      in <- a
      _  <- {
        if (in == "q\n") {
          Stream.eval(terminator.complete(()))
        } else {
          Stream(())
        }
      }
    } yield s"pure $in\n"
  }

  def program: Stream[IO, Unit] =
    Stream.eval(Deferred[IO, Unit]).flatMap { switch =>
        val switcher = Stream.eval(switch.complete(())).delayBy(10.seconds)

        val program = echoServer(switch)

        Stream(()).repeat
          .interruptWhen(switch.get.attempt)
          .concurrently(program)
          .concurrently(switcher)
    }
}
