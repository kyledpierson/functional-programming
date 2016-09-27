package parser

import data._

import scala.util.parsing.combinator.RegexParsers

object DominionParser extends RegexParsers {

  def close: Parser[Close] = "close" ^^ (s => Close())

  // NUMBER AND NAME
  def number: Parser[Int] =
    """[0-9]+""".r ^^ (_.toInt)

  def name: Parser[String] = """[A-Za-z]+""".r ^^ (_.toString)

  // CARDS
  def action: Parser[Action] =
    "cellar" ^^ (s => Cellar) | "festival" ^^ (s => Festival) | "laboratory" ^^ (s => Laboratory) |
      "market" ^^ (s => Market) | "militia" ^^ (s => Militia) | "mine" ^^ (s => Mine) |
      "moat" ^^ (s => Moat) | "remodel" ^^ (s => Remodel) | "smithy" ^^ (s => Smithy) |
      "village" ^^ (s => Village) | "woodcutter" ^^ (s => Woodcutter) | "workshop" ^^ (s => Workshop)

  def treasure: Parser[Treasure] =
    "copper" ^^ (s => Copper) | "silver" ^^ (s => Silver) | "gold" ^^ (s => Gold)

  def victory: Parser[Victory] =
    "estate" ^^ (s => Estate) | "duchy" ^^ (s => Duchy) | "province" ^^ (s => Province)

  def card: Parser[Card] = action | treasure | victory

  // PLAYS
  def act: Parser[Act] =
    "(act" ~> action ~ rep(card) <~ ")" ^^ { case action ~ cards => Act(action, cards.toVector) }

  def add: Parser[Add] =
    "(add" ~> treasure <~ ")" ^^ { case treasure => Add(treasure) }

  def buy: Parser[Buy] =
    "(buy" ~> card <~ ")" ^^ { case card => Buy(card) }

  def clean: Parser[Clean] =
    "(clean" ~> card <~ ")" ^^ { case card => Clean(Some(card)) } | "(clean)" ^^ (s => Clean(None))

  def play: Parser[Play] = act | add | buy | clean | close

  // DEFENSE
  def defend: Parser[Defend] =
    "(" ~> action <~ ")" ^^ { case action => Defend(action) }

  def discard: Parser[Discard] =
    "(discard " ~> rep(card) <~ ")" ^^ { case cards => Discard(cards.toVector) }

  def defense: Parser[Defense] = defend | discard | close

  // STATE
  def names: Parser[Vector[String]] =
    "(" ~> rep(name) <~ ")" ^^ { case names => names.toVector }

  def cards: Parser[Vector[Card]] =
    "(" ~> rep(card) <~ ")" ^^ { case cards => cards.toVector }

  def state: Parser[State] =
    "(state" ~> names ~ cards ~ cards ~ number ~ number ~ number ~ cards ~ cards ~ cards ~ cards <~ ")" ^^ { case players ~ supply ~ trash ~ actions ~ buys ~ coins ~ deck ~ hand ~ plays ~ discards => State(players, supply, trash, actions, buys, coins, deck, hand, plays, discards) }

  // NOTIFICATIONS
  def move: Parser[Move] =
    "(move" ~> state <~ ")" ^^ { case state => Move(state) }

  def moved: Parser[Moved] =
    "(moved" ~> name ~ play <~ ")" ^^ { case name ~ play => Moved(name, play) }

  def attacked: Parser[Attacked] =
    "(attacked" ~> act ~ name ~ state <~ ")" ^^ { case act ~ name ~ state => Attacked(act, name, state) }

  def defended: Parser[Defended] =
    "(defended" ~> name ~ defense <~ ")" ^^ { case name ~ defense => Defended(name, defense) }

  def notification: Parser[Notification] = move | moved | attacked | defended | close
}