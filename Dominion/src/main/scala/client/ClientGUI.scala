package client

import java.awt.{Color, Font}
import java.io.PrintWriter
import javax.swing.{BorderFactory, UIManager}

import scala.swing._
import data._
import parser.DominionParser._
import validator.Validator._

import scala.swing.event.ButtonClicked

class ClientGUI(out: PrintWriter) extends MainFrame {
  val defaultFont = new Font("Arial", Font.BOLD, 32)
  val magenta = new Color(150, 0, 0)

  UIManager.put("TextArea.font", defaultFont)
  UIManager.put("Button.font", defaultFont)
  UIManager.put("Label.font", defaultFont)

  title = "Dominion"

  def updateGUI(state: State, attack: Boolean): Unit = {
    val counts = state.supply.groupBy(w => w).mapValues(_.size)

    // LEFT BUTTONS
    val actButton = new Button("Act")
    val addButton = new Button("Add")
    val buyButton = new Button("Buy")
    val cleanButton = new Button("Clean")
    val defendButton = new Button("Defend")
    val discardButton = new Button("Discard")
    val statusLabel =
      if (attack) new Label("You are being attacked!")
      else new Label(" Actions: " + state.actions + " Coins: " + state.coins + " Buys: " + state.buys + " ")

    // RIGHT DISPLAY
    val message = new Label {
      foreground = magenta
    }
    val playText = new TextArea(10, 10) {
      editable = false
      border = BorderFactory.createEmptyBorder(10, 10, 10, 10)
    }
    val clear = new Button("Clear")
    val send = new Button("Send")

    // SUPPLY
    val victories = new FlowPanel {
      background = magenta
    }
    val treasures = new FlowPanel {
      background = magenta
    }
    val actions = new GridPanel(2, 5) {
      background = magenta
    }
    counts.foreach {
      case (card: Victory, count: Int) =>
        val cardButton =
          new Button("<html>" + card + "<br/>cost: " + card.cost + "<br/>points: " + card.points + "<br/>count: " + count + "</html>")
        victories.contents += cardButton
        listenTo(cardButton)
        reactions += {
          case ButtonClicked(`cardButton`) =>
            playText.text += " " + cardButton.text.substring(6, cardButton.text.indexOf("<br/>"))
        }
      case (card: Treasure, count: Int) =>
        val cardButton =
          new Button("<html>" + card + "<br/>cost: " + card.cost + "<br/>value: " + card.value + "<br/>count: " + count + "</html>")
        treasures.contents += cardButton
        listenTo(cardButton)
        reactions += {
          case ButtonClicked(`cardButton`) =>
            playText.text += " " + cardButton.text.substring(6, cardButton.text.indexOf("<br/>"))
        }
      case (card: Card, count: Int) =>
        val cardButton =
          new Button("<html>" + card + "<br/>cost: " + card.cost + "<br/>count: " + count + "</html>")
        actions.contents += cardButton
        listenTo(cardButton)
        reactions += {
          case ButtonClicked(`cardButton`) =>
            playText.text += " " + cardButton.text.substring(6, cardButton.text.indexOf("<br/>"))
        }
    }
    val supply = new GridPanel(3, 1) {
      border = BorderFactory.createLineBorder(Color.BLACK, 3)
      contents += victories
      contents += treasures
      contents += actions
    }

    // HAND
    val hand = new FlowPanel {
      background = Color.darkGray
    }
    state.hand.foreach(card => {
      val cardButton = new Button(card.toString)
      hand.contents += cardButton
      listenTo(cardButton)
      reactions += { case ButtonClicked(`cardButton`) => playText.text += " " + cardButton.text }
    })

    // ENTIRE BOARD
    contents = new BorderPanel {
      background = Color.darkGray

      add(new Label("Let's Play!") {
        foreground = Color.white
      }, BorderPanel.Position.North)

      add(new GridPanel(5, 1) {
        if (attack) {
          contents += defendButton
          contents += discardButton
        } else {
          contents += actButton
          contents += addButton
          contents += buyButton
          contents += cleanButton
        }
        contents += statusLabel
      }, BorderPanel.Position.West)

      add(supply, BorderPanel.Position.Center)

      add(new GridPanel(3, 1) {
        contents += message
        contents += playText
        contents += new GridPanel(1, 2) {
          contents += clear
          contents += send
        }
      }, BorderPanel.Position.East)

      add(new GridPanel(2, 1) {
        background = Color.darkGray
        contents += new Label("Hand") {
          foreground = Color.white
        }
        contents += hand
      }, BorderPanel.Position.South)
    }

    // EVENT LISTENERS
    listenTo(actButton)
    listenTo(addButton)
    listenTo(buyButton)
    listenTo(cleanButton)
    listenTo(defendButton)
    listenTo(discardButton)
    listenTo(clear)
    listenTo(send)

    reactions += {
      case ButtonClicked(`actButton`) => playText.text = "(act"
      case ButtonClicked(`addButton`) => playText.text = "(add"
      case ButtonClicked(`buyButton`) => playText.text = "(buy"
      case ButtonClicked(`cleanButton`) => playText.text = "(clean"
      case ButtonClicked(`defendButton`) => playText.text = "("
      case ButtonClicked(`discardButton`) => playText.text = "(discard"
      case ButtonClicked(`clear`) => playText.text = ""
      case ButtonClicked(`send`) =>
        val userPlay = playText.text + ")"
        playText.text = ""

        if (attack) {
          parse(defense, userPlay) match {
            case Success(result, _) =>
              if (is_valid_defense(state, result)) {
                out.write(userPlay + "\n")
                out.flush()
                message.text = ""
              } else {
                message.text = "Invalid Defense"
              }
            case _ => message.text = "Invalid Defense"
          }
        } else {
          parse(play, userPlay) match {
            case Success(result, _) =>
              if (is_valid(state, result)) {
                out.write(userPlay + "\n")
                out.flush()
                message.text = ""
              } else {
                message.text = "Invalid Play"
              }
            case _ => message.text = "Invalid Play"
          }
        }
    }
  }

  override def closeOperation = {
    out.write("close\n")
    out.flush()
  }
}
