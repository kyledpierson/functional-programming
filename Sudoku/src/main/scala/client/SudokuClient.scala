package client

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import spray.http._
import HttpMethods._

object SudokuClient extends App {
  val future: Future[HttpResponse] =
    (IO(Http) ? HttpRequest(GET, Uri("http://localhost:8080/sudoku.txt"))).mapTo[HttpResponse]

  val response = Await.result(future, 15.seconds)
  println(response.entity.data.asString)
}