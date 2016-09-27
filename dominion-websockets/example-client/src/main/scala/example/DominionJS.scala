package example

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import js.Dynamic.{global => g}
import org.scalajs.dom
import org.scalajs.dom.Event

import scalatags.JsDom._
import all._
import org.scalajs.jquery.{jQuery => $}

@JSExport
object DominionJS {
  var assetsDir: String = ""
  var wsBaseUrl: String = ""

  var client: Option[DominionClient] = None

  def signInPanel = div(id := "signInPanel") {
    form(`class` := "form-inline", "role".attr := "form")(
      div(id := "usernameForm", `class` := "form-group")(
        div(`class` := "input-group")(
          div(`class` := "input-group-addon", raw("&#9786;")),
          input(id := "username", `class` := "form-control", `type` := "text", placeholder := "Enter username")
        )
      ),
      span(style := "margin:0px 5px"),
      button(`class` := "btn btn-default", onclick := { () =>
        val input = $("#username").value().toString.trim
        if (input == "") {
          $("#usernameForm").addClass("has-error")
          dom.alert("Invalid username")
        } else {
          $("#usernameForm").removeClass("has-error")
          client = DominionClient.connect(wsBaseUrl, input).map { c =>
            $("#loginAs").text(s"Logged in as: ${c.username}")
            $("#username").value("")
            $("#signInPanel").addClass("hide")
            $("#LobbyPanel").removeClass("hide")
            c
          }
        }
        false
      })("Enter Lobby"), div(id := "error")
    )
  }

  def LobbyPanel = div(id := "LobbyPanel", `class` := "hide")(
    div(`class` := "row")(
      div(`class` := "col-md-12", style := "text-align: right;")(
        span(id := "loginAs", style := "padding: 0px 10px;"),
        button(`class` := "btn btn-default", onclick := { () =>
          signOut
        }, "Exit Lobby")
      )
    ),
    div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(
        h3(`class` := "panel-title")("Dominion Lobby")
      ),
      div(`class` := "panel-body")(
        div(id := "messages")
      )
    )
  )

  def DominionPanel = div(id := "DominionPanel", `class` := "hide")(
    div(`class` := "row")(
      div(`class` := "col-md-12", style := "text-align: right;")(
        span(id := "loginAs", style := "padding: 0px 10px;"),
        button(`class` := "btn btn-default", onclick := { () =>
          signOut
        }, "Exit Lobby")
      )
    ),
    div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(
        h3(`class` := "panel-title")("Dominion Game")
      ),
      div(`class` := "panel-body dominion-game")(
        div(`class` := "inline side", id := "play-buttons")(
          button("Act", `class` := "btn btn-primary player-btn", id := "act", onclick := { () => $("#play-text").value("act") }), br(),
          button("Add", `class` := "btn btn-primary player-btn", id := "add", onclick := { () => $("#play-text").value("add") }), br(),
          button("Buy", `class` := "btn btn-primary player-btn", id := "buy", onclick := { () => $("#play-text").value("buy") }), br(),
          button("Clean", `class` := "btn btn-primary player-btn", id := "clean", onclick := { () => $("#play-text").value("clean") }),
          br(), br(), div(id := "Info")
        ),
        div(`class` := "inline middle", id := "supply")(
          div(id := "victory-cards"), br(),
          div(id := "treasure-cards"), br(),
          div(id := "action-cards")
        ),
        div(`class` := "inline side", id := "play")(
          textarea(id := "play-text", rows := 10, readonly), br(),
          button("Clear", `class` := "btn btn-danger player-btn", id := "clear", onclick := { () => $("#play-text").value("") }),
          button("Send", `class` := "btn btn-primary player-btn", id := "send", onclick := { () =>
            client.foreach {
              _.custom_send("(" + $("#play-text").value.toString + ")")
            }
            $("#play-text").value("")
          })
        ), br(), br(),
        div(id := "hand"), div(id := "error")
      )
    )
  )

  def createSetup(humans: String, computers: String, players: js.Array[String]) = {
    div(`class` := "row")(
      div(`class` := "col-md-2")
      (button("Join", `class` := "btn btn-info", onclick := { () =>
        client.foreach {
          _.custom_send("(join (setup " + humans + " " + computers + " " + players.mkString(" ") + "))")
        }
      })),
      div(`class` := "col-md-10 middle")
      (div(`class` := "middle")("Humans: " + humans + ", Computers: " + computers + ", Current Players: " + players.mkString(", ")))
    )
  }

  def createHost() = {
    div(`class` := "row")(
      div(`class` := "col-md-2")
      (button("Host", `class` := "btn btn-primary", onclick := { () =>
        client.foreach {
          _.custom_send("(host (setup " + $("#humans").value + " " + $("#computers").value + "))")
        }
      })),
      div(`class` := "col-md-10")
      (span(`class` := "aside")("Humans:"),
        span(`class` := "aside")(select(id := "humans", `class` := "form-control aside")
        (option(value := "1", "1"), option(value := "2", "2"), option(value := "3", "3"), option(value := "4", "4"))),
        span(`class` := "aside")("Computers:"),
        span(`class` := "aside")(select(id := "computers", `class` := "form-control aside")
        (option(value := "0", "0"), option(value := "1", "1"), option(value := "2", "2"), option(value := "3", "3"))))
    )
  }

  def signOut = {
    client.foreach(_.close())
    $("#signInPanel").removeClass("hide")
    $("#LobbyPanel").addClass("hide")
    $("#DominionPanel").addClass("hide")
    $("#messages").html("")
  }

  trait DominionClient {
    val username: String

    def custom_send(msg: String)

    def send(msg: String)

    def close()
  }

  object DominionClient {
    def connect(url: String, username: String): Option[DominionClient] = {
      try {
        if (g.window.WebSocket.toString != "undefined") {
          Some(new WSDominionClient(url, username))
        } else None
      } catch {
        case e: Throwable =>
          dom.alert("Unable to connect because " + e.toString)
          None
      }
    }

    val dis = new Function2[scala.scalajs.js.Any, dom.Element, scala.scalajs.js.Any] {
      def apply(i: scala.scalajs.js.Any, e: dom.Element): scala.scalajs.js.Any = e.setAttribute("disabled", "true")
    }
    val en = new Function2[scala.scalajs.js.Any, dom.Element, scala.scalajs.js.Any] {
      def apply(i: scala.scalajs.js.Any, e: dom.Element): scala.scalajs.js.Any = e.removeAttribute("disabled")
    }
    val disable: js.Function2[scala.scalajs.js.Any, dom.Element, scala.scalajs.js.Any] = dis
    val enable: js.Function2[scala.scalajs.js.Any, dom.Element, scala.scalajs.js.Any] = en

    def receive(e: dom.MessageEvent) = {
      val data = js.JSON.parse(e.data.toString)
      println(data)

      if (data.signerror.toString != "undefined") {
        $("#error").html(data.signerror.toString)
        signOut
      } else if (data.error.toString != "undefined" && data.error.toString != "") {
        $("#play-text").value(data.error.toString)
      } else if (data.move.toString != "undefined") {
        val players = data.move.players.asInstanceOf[js.Array[String]]

        if (players.exists(s => s.toString == client.get.username)) {
          val supply = data.move.supply.asInstanceOf[js.Array[String]]
          val hand = data.move.hand.asInstanceOf[js.Array[String]]

          val victory = dom.document.getElementById("victory-cards")
          val treasure = dom.document.getElementById("treasure-cards")
          val action = dom.document.getElementById("action-cards")
          val handElem = dom.document.getElementById("hand")
          victory.innerHTML = ""
          treasure.innerHTML = ""
          action.innerHTML = ""
          handElem.innerHTML = ""

          val counts = supply.groupBy(w => w).mapValues(_.size)
          counts.foreach {
            case (card: String, count: Int) =>
              if (card.startsWith("estate") || card.startsWith("duchy") || card.startsWith("province")) {
                victory.appendChild(button(
                  card + " Count:" + count, `class` := "btn btn-success player-btn",
                  onclick := { () => $("#play-text").value($("#play-text").value
                    + " " + card.substring(0, card.indexOf(" ")))
                  }).render)
              } else if (card.startsWith("copper") || card.startsWith("silver") || card.startsWith("gold")) {
                treasure.appendChild(button(
                  card + " Count:" + count, `class` := "btn btn-warning player-btn",
                  onclick := { () => $("#play-text").value($("#play-text").value
                    + " " + card.substring(0, card.indexOf(" ")))
                  }).render)
              } else {
                action.appendChild(button(
                  card + " Count:" + count, `class` := "btn btn-info player-btn",
                  onclick := { () => $("#play-text").value($("#play-text").value
                    + " " + card.substring(0, card.indexOf(" ")))
                  }).render)
              }
          }

          $(".player-btn").each(disable)
          $("#LobbyPanel").addClass("hide")
          $("#DominionPanel").removeClass("hide")

          if (data.user.toString == client.get.username) {
            hand.foreach(card => {
              handElem.appendChild(button(
                card.toString, `class` := "btn btn-default player-btn",
                onclick := { () => $("#play-text").value($("#play-text").value + " " + card.toString) }).render)
            })
            $(".player-btn").each(enable)
            $("#Info").html("Actions: " + data.move.actions + " Buys: " + data.move.buys + " Coins: " + data.move.coins)
          }
        }
      } else if (data.score.toString != "undefined") {
        val names = data.names.asInstanceOf[js.Array[String]]
        val scores = data.scores.asInstanceOf[js.Array[String]]

        if (names.contains(client.get.username)) {
          $("#DominionPanel").html("Game Over!<br/>" + scores.mkString(" "))
        }
      } else if (data.user.toString == client.get.username && data.choose.toString != "undefined") {
        val msgElem = dom.document.getElementById("messages")
        val message = data.choose.asInstanceOf[js.Array[js.Dynamic]]

        msgElem.innerHTML = ""
        message.foreach {
          setup =>
            val humans = setup.humans.toString
            val computers = setup.computers.toString
            val players = setup.players.asInstanceOf[js.Array[String]]
            msgElem.appendChild(createSetup(humans, computers, players).render)
        }
        msgElem.appendChild(createHost().render)
      }
    }
  }

  class WSDominionClient(url: String, val username: String) extends DominionClient {
    val socket = new dom.WebSocket(url + username)
    socket.onmessage = DominionClient.receive _
    socket.onopen = { e: Event => custom_send("(games)") }

    override def custom_send(msg: String): Unit = {
      val json = js.JSON.stringify(js.Dynamic.literal(text = msg))
      socket.send(json)
    }

    override def send(msg: String): Unit = {
      val json = js.JSON.stringify(js.Dynamic.literal(text = $("#message").value()))
      socket.send(json)
    }

    override def close() = socket.close()
  }

  def ready = {
    $("#message").keypress((e: dom.KeyboardEvent) => {
      if (!e.shiftKey && e.keyCode == 13) {
        e.preventDefault()
        client.foreach {
          _.send($("#message").value().toString)
        }
        $("#message").value("")
      }
    })
  }

  @JSExport
  def main(settings: js.Dynamic) = {
    this.assetsDir = settings.assetsDir.toString
    this.wsBaseUrl = settings.wsBaseUrl.toString

    val content = dom.document.getElementById("content")
    content.appendChild(signInPanel.render)
    content.appendChild(LobbyPanel.render)
    content.appendChild(DominionPanel.render)
    ready
  }
}