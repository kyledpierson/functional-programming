package server

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.ServerSocket

import data._
import parser.DominionParser._
import validator.Validator._

import scala.Array.fill
import scala.util.Random._

object DominionServer extends App {

  case class Conn(in: BufferedReader, out: PrintWriter)

  // --------------------------------------------------------------------- //
  // ---------------------------- Game Setup ----------------------------- //
  // --------------------------------------------------------------------- //

  val server = new ServerSocket(4000)

  val player = server.accept()
  val playerOut = new PrintWriter(player.getOutputStream)
  val playerIn = new BufferedReader(new InputStreamReader(player.getInputStream))

  val comp = server.accept()
  val compOut = new PrintWriter(comp.getOutputStream)
  val compIn = new BufferedReader(new InputStreamReader(comp.getInputStream))

  val deck = (fill(7)(Copper) ++ fill(3)(Estate)).toVector
  val supply = (fill(10)(Cellar) ++ fill(10)(Moat) ++
    fill(10)(Village) ++ fill(10)(Woodcutter) ++ fill(10)(Workshop) ++
    fill(10)(Militia) ++ fill(10)(Remodel) ++ fill(10)(Smithy) ++
    fill(10)(Market) ++ fill(10)(Mine) ++
    fill(20)(Copper) ++ fill(20)(Silver) ++ fill(20)(Gold) ++
    fill(8)(Estate) ++ fill(8)(Duchy) ++ fill(8)(Province)).toVector

  val deck1 = shuffle(deck)
  val deck2 = shuffle(deck)

  val current_conn = Conn(playerIn, playerOut)
  val current_state = State(Vector("player", "computer"), supply, Vector(),
    1, 1, 0, deck1.drop(5), deck1.take(5), Vector(), Vector())
  val waiting_conn = Conn(compIn, compOut)
  val waiting_state = State(Vector("computer", "player"), supply, Vector(),
    1, 1, 0, deck2.drop(5), deck2.take(5), Vector(), Vector())

  val notification = Move(current_state)

  playerOut.write(notification + "\n")
  playerOut.flush()
  println(notification)
  listen(current_conn, current_state, waiting_conn, waiting_state)

  // --------------------------------------------------------------------- //
  // ----------------------------- Functions ----------------------------- //
  // --------------------------------------------------------------------- //

  def listen(current_conn: Conn, current_state: State, waiting_conn: Conn, waiting_state: State): Unit = {
    val input = current_conn.in.readLine
    println(input)

    val parsed_play = parse(play, input)

    parsed_play match {
      case Success(client_play, _) =>
        if (client_play.isInstanceOf[Close]) close()
        if (is_valid(current_state, client_play)) {
          val updated_states = update_states(current_state, current_conn, waiting_state, waiting_conn, client_play)
          val updated_state = updated_states._1
          val updated_waiting = updated_states._2
          val moved = Moved(updated_state.players.head, client_play)

          current_conn.out.write(moved + "\n")
          current_conn.out.flush()
          waiting_conn.out.write(moved + "\n")
          waiting_conn.out.flush()
          println(moved)

          client_play match {
            case Clean(card) =>
              if (updated_state.supply.contains(Province)) {
                val move = Move(updated_waiting)
                waiting_conn.out.write(move + "\n")
                waiting_conn.out.flush()
                println(move)
                listen(waiting_conn, updated_waiting, current_conn, updated_state)
              } else {
                val player_score =
                  (updated_state.hand ++ updated_state.deck ++ updated_state.discards)
                    .filter(_.isInstanceOf[Victory]).foldLeft(0)(_ + _.asInstanceOf[Victory].points)

                val waiting_score =
                  (updated_waiting.hand ++ updated_waiting.deck ++ updated_waiting.discards)
                    .filter(_.isInstanceOf[Victory]).foldLeft(0)(_ + _.asInstanceOf[Victory].points)

                println("Game Over\n"
                  + updated_state.players.head + ": " + player_score + "\n"
                  + waiting_state.players.head + ": " + waiting_score)
                close()
              }
            case _ =>
              val move = Move(updated_state)
              current_conn.out.write(move + "\n")
              current_conn.out.flush()
              println(move)
              listen(current_conn, updated_state, waiting_conn, updated_waiting)
          }
        } else {
          throw new Exception(client_play + " is an invalid play in state " + current_state)
        }
      case Failure(result, _) => throw new Exception(result)
      case Error(result, _) => throw new Exception(result)
    }
  }

  def update_states(cur_state: State, cur_conn: Conn, wait_state: State, wait_conn: Conn, play: Play): (State, State) = {
    play match {
      case Act(action, cards) =>
        val state = cur_state.copy(
          actions = cur_state.actions - 1,
          hand = cur_state.hand.diff(Vector(action)),
          plays = cur_state.plays :+ action)

        action match {
          case Cellar =>
            val new_state =
              draw_cards(state.copy(hand = state.hand.diff(cards), discards = state.discards ++ cards), cards.length)

            (new_state, wait_state)

          case Militia =>
            val defended_state = defend_militia(wait_state, cur_conn, wait_conn)
            (apply_benefit(state, action.benefit.get), defended_state)

          case Mine =>
            (state.copy(
              supply = state.supply.diff(Vector(cards(1))),
              trash = state.trash :+ cards.head,
              hand = state.hand.diff(Vector(cards.head)) :+ cards(1)),

              wait_state.copy(
                supply = wait_state.supply.diff(Vector(cards(1))),
                trash = wait_state.trash :+ cards.head))

          case Remodel =>
            (state.copy(
              supply = state.supply.diff(Vector(cards(1))),
              trash = state.trash :+ cards.head,
              hand = state.hand.diff(Vector(cards.head)),
              discards = state.discards :+ cards(1)),

              wait_state.copy(
                supply = wait_state.supply.diff(Vector(cards(1))),
                trash = wait_state.trash :+ cards.head))

          case Workshop =>
            (state.copy(
              supply = state.supply.diff(Vector(cards.head)),
              discards = state.discards :+ cards.head),

              wait_state.copy(supply = wait_state.supply.diff(Vector(cards.head))))
          case _ =>
            (apply_benefit(state, action.benefit.get), wait_state)
        }

      case Add(treasure) =>
        (cur_state.copy(
          coins = cur_state.coins + treasure.value,
          hand = cur_state.hand.diff(Vector(treasure)),
          plays = cur_state.plays :+ treasure), wait_state)

      case Buy(card) =>
        (cur_state.copy(
          supply = cur_state.supply.diff(Vector(card)),
          actions = 0, // This is done to prevent actions after buying
          buys = cur_state.buys - 1,
          coins = cur_state.coins - card.cost,
          discards = cur_state.discards :+ card),

          wait_state.copy(supply = wait_state.supply.diff(Vector(card))))

      case Clean(card_option) =>
        val new_state = draw_cards(
          cur_state.copy(actions = 1, buys = 1, coins = 0, hand = Vector(), plays = Vector(),
            discards = cur_state.discards ++ cur_state.plays ++ cur_state.hand), 5)

        (new_state, wait_state)
    }
  }

  def draw_cards(state: State, num: Int): State = {
    if (state.deck.length < num) {
      val new_deck = state.deck ++ shuffle(state.discards)
      state.copy(
        deck = new_deck.drop(num),
        hand = state.hand ++ new_deck.take(num),
        discards = Vector()
      )
    } else {
      state.copy(
        deck = state.deck.drop(num),
        hand = state.hand ++ state.deck.take(num)
      )
    }
  }

  def apply_benefit(state: State, benefit: Benefit): State = {
    val new_state = draw_cards(state, benefit.cards)

    new_state.copy(
      actions = new_state.actions + benefit.actions,
      buys = new_state.buys + benefit.buys,
      coins = new_state.coins + benefit.coins
    )
  }

  def defend_militia(wait_state: State, cur_conn: Conn, wait_conn: Conn): State = {
    val notify =
      Attacked(Act(Militia, Vector()), wait_state.players(1), wait_state.copy(actions = 0, coins = 0, buys = 0))

    wait_conn.out.write(notify + "\n")
    wait_conn.out.flush()
    println(notify)

    val input = wait_conn.in.readLine
    println(input)

    val parsed_defense = parse(defense, input)

    parsed_defense match {
      case Success(result, _) =>
        if (is_valid_defense(wait_state, result)) {
          val defend_notify = Defended(wait_state.players.head, result)

          cur_conn.out.write(defend_notify + "\n")
          cur_conn.out.flush()
          println(defend_notify)

          result match {
            case Defend(action) => wait_state
            case Discard(cards) =>
              wait_state.copy(
                hand = wait_state.hand.diff(cards),
                discards = wait_state.discards ++ cards
              )
          }
        } else {
          throw new Exception(result + " is an invalid defense in state " + wait_state)
        }
      case Failure(result, _) => throw new Exception(result)
      case Error(result, _) => throw new Exception(result)
    }
  }

  def close() = {
    playerOut.write("close")
    compOut.write("close")
    playerOut.flush()
    compOut.flush()

    player.close()
    comp.close()
    server.close()
    System.exit(0)
  }
}
