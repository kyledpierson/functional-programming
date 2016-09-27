package client

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

import data._
import parser.DominionParser._
import validator.Validator._

object DominionClient extends App {
  val clientSocket = new Socket("localhost", 4000)
  val out = new PrintWriter(clientSocket.getOutputStream)
  val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))

  val gui = new ClientGUI(out)
  gui.visible = true

  run_client()

  def run_client(): Unit = {
    val notify = parse(notification, in.readLine)

    notify match {
      case Success(result, _) =>
        result match {
          case Move(state) =>
            gui.updateGUI(state, attack = false)
          case Attacked(action, name, state) =>
            gui.updateGUI(state, attack = true)
          case Close() => System.exit(0)
          case _ => // Moved or Defended, do nothing
        }
      case Failure(result, _) => throw new Exception(result)
      case Error(result, _) => throw new Exception(result)
    }

    run_client()
  }
}