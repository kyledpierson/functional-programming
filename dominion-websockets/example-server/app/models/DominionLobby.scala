package models

import scala.Array._
import scala.util.Random
import scala.util.Random._
import scala.concurrent.Future
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.collection.immutable._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import play.api.libs.json.{JsObject, _}
import play.api.Play.current
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.concurrent.Execution.Implicits._
import models.DominionParser._
import models.DominionValidator._

object DominionLobby {
  implicit val timeout = Timeout(1 second)
  lazy val default = Akka.system.actorOf(Props[DominionLobby])

  def connect(username: String): Future[(Iteratee[JsValue, _], Enumerator[JsValue])] = {
    (default ? Connect(username)).map {
      case Connected(name, enumerator) =>
        (Iteratee.foreach[JsValue] { event => talk(name, (event \ "text").as[String]) }, enumerator)
      case CannotConnect(error) =>
        val iteratee = Done[JsValue, Unit]((), Input.EOF)
        val enumerator = Enumerator[JsValue](JsObject(Seq("signerror" -> JsString(error))))
          .andThen(Enumerator.enumInput(Input.EOF))
        (iteratee, enumerator)
    }
  }

  def talk(name: String, message: String): Unit = {
    parse(choice, message) match {
      case Success(result, _) =>
        result match {
          case Games(blank) => default ! Games(name)
          case Host(blank, setup) => default ! Host(name, setup)
          case Join(blank, setup) => default ! Join(name, setup)
        }
      case _ =>
        parse(play, message) match {
          case Success(result, _) =>
            result match {
              case Act(blank, action, cards) => default ! Act(name, action, cards)
              case Add(blank, treasure) => default ! Add(name, treasure)
              case Buy(blank, cards) => default ! Buy(name, cards)
              case Clean(blank, card) => default ! Clean(name, card)
            }
          case _ =>
        }
    }
  }
}


class DominionLobby extends Actor {
  implicit val timeout = Timeout(1 second)
  val (dominionEnumerator, dominionChannel) = Concurrent.broadcast[JsValue]

  val deck = (fill(7)(Copper) ++ fill(3)(Estate)).toVector
  val supply = (fill(10)(Cellar) ++ fill(10)(Festival) ++
    fill(10)(Laboratory) ++ fill(10)(Market) ++ fill(10)(Mine) ++
    fill(10)(Remodel) ++ fill(10)(Smithy) ++ fill(10)(Village) ++
    fill(10)(Woodcutter) ++ fill(10)(Workshop) ++
    fill(20)(Copper) ++ fill(20)(Silver) ++ fill(20)(Gold) ++
    fill(8)(Estate) ++ fill(8)(Duchy) ++ fill(8)(Province)).toVector

  def receive = {
    case Connect(name) =>
      val new_users = Vector(name)
      sender ! Connected(name, dominionEnumerator)
      context.become(new_room(0, new_users, Vector(), HashMap(), HashMap()))
  }

  def new_room(gid: Int,
               users: Vector[String],
               setups: Vector[Setup],
               users_to_gid: HashMap[String, Int],
               gid_to_game: HashMap[Int, Game]): Receive = {
    case Connect(name) =>
      if (users.contains(name) || users_to_gid.contains(name) || name.startsWith("Computer")) {
        sender ! CannotConnect("This username is already used")
      } else {
        val new_users = users :+ name
        sender ! Connected(name, dominionEnumerator)
        context.become(new_room(gid, new_users, setups, users_to_gid, gid_to_game))
      }

    case Games(name) =>
      dominionChannel.push(make_json(name, Choose(setups), ""))

    case Host(name, setup) =>
      val new_setup = setup.copy(players = setup.players :+ name)
      val updated =
        if (setup.computers > 0)
          new_setup.copy(players = new_setup.players ++ (1 to setup.computers).map(i => "Computer" + gid + i))
        else new_setup

      val new_setups = setups :+ updated

      val ret = try_start(gid, users, new_setups, updated, users_to_gid, gid_to_game, new_setups.size - 1)
      context.become(new_room(ret._1, ret._2, ret._3, ret._4, ret._5))

    case Join(name, setup) =>
      val index = setups.indexOf(setup)
      if (index > -1) {
        val new_setup = setups(index).copy(players = setups(index).players :+ name)
        val ret = try_start(gid, users, setups, new_setup, users_to_gid, gid_to_game, index)
        context.become(new_room(ret._1, ret._2, ret._3, ret._4, ret._5))
      }

    case Act(name, action, cards) =>
      val g = users_to_gid(name)
      val states = do_play(gid_to_game(g), name, Act(name, action, cards))

      if (states.nonEmpty)
        context.become(new_room(gid, users, setups, users_to_gid, gid_to_game.updated(g, Game(states.get))))

    case Add(name, treasure) =>
      val g = users_to_gid(name)
      val states = do_play(gid_to_game(g), name, Add(name, treasure))

      if (states.nonEmpty)
        context.become(new_room(gid, users, setups, users_to_gid, gid_to_game.updated(g, Game(states.get))))

    case Buy(name, card) =>
      val g = users_to_gid(name)
      val states = do_play(gid_to_game(g), name, Buy(name, card))

      if (states.nonEmpty)
        context.become(new_room(gid, users, setups, users_to_gid, gid_to_game.updated(g, Game(states.get))))

    case Clean(name, card) =>
      val play = Clean(name, card)
      val g = users_to_gid(name)
      val game = gid_to_game(g)

      if (is_valid(game.states.head, play)) {
        val new_states = update_states(game.states, play)
        val next = new_states.head

        val moved = Moved(name, play)
        next.players.foreach(user => dominionChannel.push(make_json(user, moved, "")))

        if (next.supply.contains(Province)) {
          val comp_states = do_computers(new_states)

          dominionChannel.push(make_json(comp_states.head.players.head, Move(comp_states.head), ""))
          context.become(new_room(gid, users, setups, users_to_gid, gid_to_game.updated(g, Game(comp_states))))
        } else {
          val scores = new_states.foldLeft(Vector[String]())(
            (sc, st) => sc :+ st.players.head + ": " + (st.hand ++ st.deck ++ st.discards)
              .filter(_.isInstanceOf[Victory]).foldLeft(0)(_ + _.asInstanceOf[Victory].points))

          dominionChannel.push(Json.obj("user" -> name, "names" -> next.players, "scores" -> scores, "score" -> "true"))
          context.become(new_room(gid, users, setups, users_to_gid -- next.players, gid_to_game - g))
        }
      }
  }

  def do_computers(states: Vector[State]): Vector[State] = {
    val state = states.head

    if (state.players.head.startsWith("Computer")) {
      val play = make_play(state)
      val new_states = update_states(states, play)

      val moved = Moved(state.players.head, play)
      state.players.foreach(user => dominionChannel.push(make_json(user, moved, "")))

      do_computers(new_states)
    } else {
      states
    }
  }

  def try_start(gid: Int,
                users: Vector[String],
                setups: Vector[Setup],
                setup: Setup,
                users_to_gid: HashMap[String, Int],
                gid_to_game: HashMap[Int, Game],
                index: Int): (Int, Vector[String], Vector[Setup], HashMap[String, Int], HashMap[Int, Game]) = {
    val start_game = setup.players.length == setup.humans + setup.computers

    val new_users =
      if (start_game) users.diff(setup.players)
      else users
    val new_setups =
      if (start_game) setups.take(index) ++ setups.drop(index + 1)
      else setups.updated(index, setup)

    val game = Game(make_states(Vector(), setup.players))
    val first_name = game.states.head.players.head
    val state = game.states.head

    val new_gid = if (start_game) gid + 1 else gid
    val u2g =
      if (start_game) state.players.foldLeft(users_to_gid)(
        (m, n) => if (!n.startsWith("Computer")) m + (n -> new_gid) else m)
      else users_to_gid
    val g2g =
      if (start_game) gid_to_game + (new_gid -> game)
      else gid_to_game

    if (start_game) dominionChannel.push(make_json(first_name, Move(state), ""))
    new_users.foreach(username => dominionChannel.push(make_json(username, Choose(new_setups), "")))
    (new_gid, new_users, new_setups, u2g, g2g)
  }

  def make_states(done: Vector[String], remaining: Vector[String]): Vector[State] = {
    if (remaining.isEmpty) Vector()
    else {
      val d = shuffle(deck)
      val new_remaining = remaining.tail
      val new_done = done :+ remaining.head
      State(remaining ++ done, supply, Vector(), 1, 1, 0,
        d.drop(5), d.take(5), Vector(), Vector()) +: make_states(new_done, new_remaining)
    }
  }

  def do_play(game: Game,
              name: String,
              play: Play): Option[Vector[State]] = {
    val state = game.states.head

    if (name == state.players.head && is_valid(state, play)) {
      val updated_states = update_states(game.states, play)

      val moved = Moved(name, play)
      updated_states.head.players.foreach(user => dominionChannel.push(make_json(user, moved, "")))

      dominionChannel.push(make_json(name, Move(updated_states.head), ""))
      Some(updated_states)
    } else {
      dominionChannel.push(make_json(name, Move(state), "Invalid Move"))
      None
    }
  }

  def update_states(states: Vector[State], play: Play): Vector[State] = {
    play match {
      case Act(name, action, cards) =>
        val state = states.head.copy(
          actions = states.head.actions - 1,
          hand = states.head.hand.diff(Vector(action)),
          plays = states.head.plays :+ action)

        action match {
          case Cellar =>
            val new_state =
              draw_cards(state.copy(hand = state.hand.diff(cards), discards = state.discards ++ cards), cards.length)

            new_state +: states.tail

          case Mine =>
            val new_state = state.copy(
              supply = state.supply.diff(Vector(cards(1))),
              trash = state.trash :+ cards.head,
              hand = state.hand.diff(Vector(cards.head)) :+ cards(1))

            val new_states = states.tail.foldLeft(Vector[State]())((v, s) =>
              v :+ s.copy(
                supply = s.supply.diff(Vector(cards(1))),
                trash = s.trash :+ cards.head))

            new_state +: new_states

          case Remodel =>
            val new_state = state.copy(
              supply = state.supply.diff(Vector(cards(1))),
              trash = state.trash :+ cards.head,
              hand = state.hand.diff(Vector(cards.head)),
              discards = state.discards :+ cards(1))

            val new_states = states.tail.foldLeft(Vector[State]())((v, s) =>
              v :+ s.copy(
                supply = s.supply.diff(Vector(cards(1))),
                trash = s.trash :+ cards.head))

            new_state +: new_states

          case Workshop =>
            val new_state = state.copy(
              supply = state.supply.diff(Vector(cards.head)),
              discards = state.discards :+ cards.head)

            val new_states = states.tail.foldLeft(Vector[State]())((v, s) =>
              v :+ s.copy(supply = s.supply.diff(Vector(cards.head))))

            new_state +: new_states
          case _ =>
            apply_benefit(state, action.benefit.get) +: states.tail
        }

      case Add(name, treasure) =>
        states.head.copy(
          coins = states.head.coins + treasure.value,
          hand = states.head.hand.diff(Vector(treasure)),
          plays = states.head.plays :+ treasure) +: states.tail

      case Buy(name, card) =>
        val new_state = states.head.copy(
          supply = states.head.supply.diff(Vector(card)),
          actions = 0, // This is done to prevent actions after buying
          buys = states.head.buys - 1,
          coins = states.head.coins - card.cost,
          discards = states.head.discards :+ card)

        val new_states = states.tail.foldLeft(Vector[State]())((v, s) =>
          v :+ s.copy(supply = s.supply.diff(Vector(card))))

        new_state +: new_states

      case Clean(name, card_option) =>
        states.tail :+ draw_cards(
          states.head.copy(actions = 1, buys = 1, coins = 0, hand = Vector(), plays = Vector(),
            discards = states.head.discards ++ states.head.plays ++ states.head.hand), 5)
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

  def make_play(st: State): Play = {
    if (st.actions > 0) {
      // Try an action that gives more actions
      if (is_valid(st, Act(st.players.head, Festival, Vector()))) return Act(st.players.head, Festival, Vector())
      if (is_valid(st, Act(st.players.head, Laboratory, Vector()))) return Act(st.players.head, Laboratory, Vector())
      if (is_valid(st, Act(st.players.head, Market, Vector()))) return Act(st.players.head, Market, Vector())
      if (is_valid(st, Act(st.players.head, Village, Vector()))) return Act(st.players.head, Village, Vector())

      // Mining is always good
      if (st.hand.contains(Mine)) {
        if (is_valid(st, Act(st.players.head, Mine, Vector(Silver, Gold)))) return Act(st.players.head, Mine, Vector(Silver, Gold))
        if (is_valid(st, Act(st.players.head, Mine, Vector(Copper, Silver)))) return Act(st.players.head, Mine, Vector(Copper, Silver))
      }

      // Try remodeling
      if (st.hand.contains(Remodel)) {
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Smithy, Gold)))) return Act(st.players.head, Remodel, Vector(Smithy, Gold))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Remodel, Gold)))) return Act(st.players.head, Remodel, Vector(Remodel, Gold))

        if (is_valid(st, Act(st.players.head, Remodel, Vector(Workshop, Festival)))) return Act(st.players.head, Remodel, Vector(Workshop, Festival))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Workshop, Laboratory)))) return Act(st.players.head, Remodel, Vector(Workshop, Laboratory))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Workshop, Market)))) return Act(st.players.head, Remodel, Vector(Workshop, Market))

        if (is_valid(st, Act(st.players.head, Remodel, Vector(Woodcutter, Festival)))) return Act(st.players.head, Remodel, Vector(Woodcutter, Festival))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Woodcutter, Laboratory)))) return Act(st.players.head, Remodel, Vector(Woodcutter, Laboratory))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Woodcutter, Market)))) return Act(st.players.head, Remodel, Vector(Woodcutter, Market))

        if (is_valid(st, Act(st.players.head, Remodel, Vector(Village, Festival)))) return Act(st.players.head, Remodel, Vector(Village, Festival))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Village, Laboratory)))) return Act(st.players.head, Remodel, Vector(Village, Laboratory))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Village, Market)))) return Act(st.players.head, Remodel, Vector(Village, Market))

        if (is_valid(st, Act(st.players.head, Remodel, Vector(Workshop, Mine)))) return Act(st.players.head, Remodel, Vector(Workshop, Mine))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Woodcutter, Mine)))) return Act(st.players.head, Remodel, Vector(Woodcutter, Mine))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Village, Mine)))) return Act(st.players.head, Remodel, Vector(Village, Mine))

        if (is_valid(st, Act(st.players.head, Remodel, Vector(Estate, Remodel)))) return Act(st.players.head, Remodel, Vector(Estate, Remodel))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Estate, Smithy)))) return Act(st.players.head, Remodel, Vector(Estate, Smithy))

        if (is_valid(st, Act(st.players.head, Remodel, Vector(Cellar, Remodel)))) return Act(st.players.head, Remodel, Vector(Cellar, Remodel))
        if (is_valid(st, Act(st.players.head, Remodel, Vector(Cellar, Smithy)))) return Act(st.players.head, Remodel, Vector(Cellar, Smithy))
      }

      // Try the smithy
      if (is_valid(st, Act(st.players.head, Smithy, Vector()))) return Act(st.players.head, Smithy, Vector())

      // Woodcutter or Workshop might help
      if (is_valid(st, Act(st.players.head, Woodcutter, Vector()))) return Act(st.players.head, Woodcutter, Vector())

      if (st.hand.contains(Workshop)) {
        if (is_valid(st, Act(st.players.head, Workshop, Vector(Remodel)))) return Act(st.players.head, Workshop, Vector(Remodel))
        if (is_valid(st, Act(st.players.head, Workshop, Vector(Smithy)))) return Act(st.players.head, Workshop, Vector(Smithy))
      }

      // Cellar
      val discard_cards =
        if (st.actions > 1) st.hand.filter(_.isInstanceOf[Victory])
        else st.hand.diff(Vector(Cellar)).filterNot(_.isInstanceOf[Treasure])

      if (is_valid(st, Act(st.players.head, Cellar, discard_cards)))
        return Act(st.players.head, Cellar, discard_cards)
    }

    // You still have buys
    if (st.buys > 0) {

      // Add treasures if you can
      val treasure = st.hand.find(_.isInstanceOf[Treasure])
      if (treasure.isDefined) return Add(st.players.head, treasure.get.asInstanceOf[Treasure])

      // Buy a card if you can
      if (st.coins >= 8) return Buy(st.players.head, Province)
      if (st.supply.count({ case Province => true case _ => false }) < 3 &&
        st.supply.contains(Duchy) && st.coins >= 5) return Buy(st.players.head, Duchy)

      val cards = st.supply.filter(_.cost <= st.coins).filterNot(_.isInstanceOf[Victory])
      val maxCost = cards.maxBy(_.cost).cost
      val options = cards.filter(_.cost == maxCost).distinct

      // Randomly resolve ties
      if (options.nonEmpty) return Buy(st.players.head, Random.shuffle(options).head)
    }

    // Can't do anything
    if (st.hand.nonEmpty) Clean(st.players.head, Some(st.hand.head))
    else Clean(st.players.head, None)
  }

  def make_json(name: String, obj: Object, error: String): JsObject = {
    obj match {
      case Choose(setups) => Json.obj(
        "user" -> name,
        "error" -> error,
        "choose" -> setups.map(setup => Json.obj(
          "humans" -> setup.humans,
          "computers" -> setup.computers,
          "players" -> setup.players)))
      case Close() => Json.obj(
        "user" -> name,
        "error" -> error,
        "close" -> "close")
      case Move(state) => Json.obj(
        "user" -> name,
        "error" -> error,
        "move" -> Json.obj(
          "players" -> state.players,
          "supply" -> state.supply.map(card => card.toString + " Cost:" + card.cost),
          "trash" -> state.trash.map(card => card.toString),
          "actions" -> state.actions,
          "buys" -> state.buys,
          "coins" -> state.coins,
          "deck" -> state.deck.map(card => card.toString),
          "hand" -> state.hand.map(card => card.toString),
          "plays" -> state.plays.map(card => card.toString),
          "discards" -> state.discards.map(card => card.toString)))
      case Moved(mover, play) => Json.obj(
        "user" -> name,
        "error" -> error,
        "moved" -> Json.obj(
          "name" -> mover,
          "play" -> play.toString
        )
      )
    }
  }
}