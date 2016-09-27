package server

import akka.actor.Actor
import server.SudokuGenerator._
import spray.routing._
import spray.http.HttpHeaders.RawHeader
import spray.httpx.marshalling.ToResponseMarshallable

class SudokuServiceActor extends Actor with SudokuService {
  def actorRefFactory = context
  def receive = runRoute(route)
}

trait SudokuService extends HttpService {
  val route =
    path("sudoku.txt") {
      get {
        respondWithHeader(RawHeader("Content-Type", "text/plain; charset=us-ascii")) {
          complete {
            ToResponseMarshallable.isMarshallable("3 3\n" + toPuzzleString(initializePuzzle(9, 3, 3, 30)))
          }
        }
      }
    }
}