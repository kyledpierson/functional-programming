package controllers

import models.DominionLobby
import play.api.libs.json.JsValue
import play.api.mvc.{WebSocket, Action, Controller}
import play.api.libs.concurrent.Execution.Implicits._

object DominionController extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.dominion())
  }

  def dominionWS(name: String) = WebSocket.tryAccept[JsValue] { request =>
    DominionLobby.connect(name).map { io => Right(io) }.recover { case e => Left(Ok(e)) }
  }
}
