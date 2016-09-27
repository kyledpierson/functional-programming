package client

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

import scala.util.Random
import data._
import parser.DominionParser._
import validator.Validator._

object DominionAI extends App {
  val client_socket = new Socket("localhost", 4000)
  val out = new PrintWriter(client_socket.getOutputStream)
  val in = new BufferedReader(new InputStreamReader(client_socket.getInputStream))

  run_client()

  def run_client(): Unit = {
    val notify = parse(notification, in.readLine)

    notify match {
      case Success(result, _) =>
        result match {
          case Move(state) =>
            val play = make_play(state)
            out.write(play + "\n")
            out.flush()
          case Attacked(action, name, state) =>
            val defense = make_defense(state)
            out.write(defense + "\n")
            out.flush()
          case Close() => System.exit(0)
          case _ => // Do Nothing?
        }
      case Failure(result, _) => throw new Exception(result)
      case Error(result, _) => throw new Exception(result)
    }

    run_client()
  }

  def make_play(st: State): Play = {

    if (st.actions > 0) {
      // Try an action that gives more actions
      if (is_valid(st, Act(Festival, Vector()))) return Act(Festival, Vector())
      if (is_valid(st, Act(Laboratory, Vector()))) return Act(Laboratory, Vector())
      if (is_valid(st, Act(Market, Vector()))) return Act(Market, Vector())
      if (is_valid(st, Act(Village, Vector()))) return Act(Village, Vector())

      // Mining is always good
      if (st.hand.contains(Mine)) {
        if (is_valid(st, Act(Mine, Vector(Silver, Gold)))) return Act(Mine, Vector(Silver, Gold))
        if (is_valid(st, Act(Mine, Vector(Copper, Silver)))) return Act(Mine, Vector(Copper, Silver))
      }

      // Try remodeling
      if (st.hand.contains(Remodel)) {
        if (is_valid(st, Act(Remodel, Vector(Smithy, Gold)))) return Act(Remodel, Vector(Smithy, Gold))
        if (is_valid(st, Act(Remodel, Vector(Remodel, Gold)))) return Act(Remodel, Vector(Remodel, Gold))
        if (is_valid(st, Act(Remodel, Vector(Militia, Gold)))) return Act(Remodel, Vector(Militia, Gold))

        if (is_valid(st, Act(Remodel, Vector(Workshop, Festival)))) return Act(Remodel, Vector(Workshop, Festival))
        if (is_valid(st, Act(Remodel, Vector(Workshop, Laboratory)))) return Act(Remodel, Vector(Workshop, Laboratory))
        if (is_valid(st, Act(Remodel, Vector(Workshop, Market)))) return Act(Remodel, Vector(Workshop, Market))

        if (is_valid(st, Act(Remodel, Vector(Woodcutter, Festival)))) return Act(Remodel, Vector(Woodcutter, Festival))
        if (is_valid(st, Act(Remodel, Vector(Woodcutter, Laboratory)))) return Act(Remodel, Vector(Woodcutter, Laboratory))
        if (is_valid(st, Act(Remodel, Vector(Woodcutter, Market)))) return Act(Remodel, Vector(Woodcutter, Market))

        if (is_valid(st, Act(Remodel, Vector(Village, Festival)))) return Act(Remodel, Vector(Village, Festival))
        if (is_valid(st, Act(Remodel, Vector(Village, Laboratory)))) return Act(Remodel, Vector(Village, Laboratory))
        if (is_valid(st, Act(Remodel, Vector(Village, Market)))) return Act(Remodel, Vector(Village, Market))

        if (is_valid(st, Act(Remodel, Vector(Workshop, Mine)))) return Act(Remodel, Vector(Workshop, Mine))
        if (is_valid(st, Act(Remodel, Vector(Woodcutter, Mine)))) return Act(Remodel, Vector(Woodcutter, Mine))
        if (is_valid(st, Act(Remodel, Vector(Village, Mine)))) return Act(Remodel, Vector(Village, Mine))

        if (is_valid(st, Act(Remodel, Vector(Estate, Militia)))) return Act(Remodel, Vector(Estate, Militia))
        if (is_valid(st, Act(Remodel, Vector(Estate, Remodel)))) return Act(Remodel, Vector(Estate, Remodel))
        if (is_valid(st, Act(Remodel, Vector(Estate, Smithy)))) return Act(Remodel, Vector(Estate, Smithy))

        if (is_valid(st, Act(Remodel, Vector(Cellar, Militia)))) return Act(Remodel, Vector(Cellar, Militia))
        if (is_valid(st, Act(Remodel, Vector(Cellar, Remodel)))) return Act(Remodel, Vector(Cellar, Remodel))
        if (is_valid(st, Act(Remodel, Vector(Cellar, Smithy)))) return Act(Remodel, Vector(Cellar, Smithy))

        if (is_valid(st, Act(Remodel, Vector(Moat, Militia)))) return Act(Remodel, Vector(Moat, Militia))
        if (is_valid(st, Act(Remodel, Vector(Moat, Remodel)))) return Act(Remodel, Vector(Moat, Remodel))
        if (is_valid(st, Act(Remodel, Vector(Moat, Smithy)))) return Act(Remodel, Vector(Moat, Smithy))
      }

      // Try the smithy
      if (is_valid(st, Act(Militia, Vector()))) return Act(Militia, Vector())
      if (is_valid(st, Act(Smithy, Vector()))) return Act(Smithy, Vector())

      // Woodcutter or Workshop might help
      if (is_valid(st, Act(Woodcutter, Vector()))) return Act(Woodcutter, Vector())

      if (st.hand.contains(Workshop)) {
        if (is_valid(st, Act(Workshop, Vector(Militia)))) return Act(Workshop, Vector(Militia))
        if (is_valid(st, Act(Workshop, Vector(Remodel)))) return Act(Workshop, Vector(Remodel))
        if (is_valid(st, Act(Workshop, Vector(Smithy)))) return Act(Workshop, Vector(Smithy))
      }

      // Moat or Cellar, depending on your hand
      val discard_cards =
        if (st.actions > 1) st.hand.filter(_.isInstanceOf[Victory])
        else st.hand.diff(Vector(Cellar)).filterNot(_.isInstanceOf[Treasure])

      val valid_cellar = is_valid(st, Act(Cellar, discard_cards))

      if ((!valid_cellar || discard_cards.length < 3) && is_valid(st, Act(Moat, Vector())))
        return Act(Moat, Vector())
      if (valid_cellar)
        return Act(Cellar, discard_cards)
    }

    // You still have buys
    if (st.buys > 0) {

      // Add treasures if you can
      val treasure = st.hand.find(_.isInstanceOf[Treasure])
      if (treasure.isDefined) return Add(treasure.get.asInstanceOf[Treasure])

      // Buy a card if you can
      if (st.coins >= 8) return Buy(Province)
      if (st.supply.count({ case Province => true case _ => false }) < 3 &&
        st.supply.contains(Duchy) && st.coins >= 5) return Buy(Duchy)

      val cards = st.supply.filter(_.cost <= st.coins).filterNot(_.isInstanceOf[Victory])
      val maxCost = cards.maxBy(_.cost).cost
      val options = cards.filter(_.cost == maxCost).distinct

      // Randomly resolve ties
      if (options.nonEmpty) return Buy(Random.shuffle(options).head)
    }

    // Can't do anything
    if (st.hand.nonEmpty) Clean(Some(st.hand.head))
    else Clean(None)
  }

  def make_defense(state: State): Defense = {
    if (is_valid_defense(state, Defend(Moat))) Defend(Moat)
    else {
      val ordered_hand = state.hand.filter(_.isInstanceOf[Victory]) ++
        state.hand.filterNot(_.isInstanceOf[Victory]).sortBy(card => card.cost)

      Discard(ordered_hand.dropRight(3))
    }
  }
}
