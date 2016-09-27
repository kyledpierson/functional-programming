package server

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http

import scala.concurrent.duration._

object SudokuServer extends App {
  implicit val timeout = Timeout(5.seconds)
  implicit val system = ActorSystem("on-spray-can")
  val service = system.actorOf(Props[SudokuServiceActor], "sudoku-service")

  IO(Http) ? Http.Bind(service, interface = "localhost", port = 8080)
}
