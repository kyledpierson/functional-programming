package models

import scala.util.parsing.combinator.RegexParsers

object DominionParser extends RegexParsers {

  // GAME
  def setup: Parser[Setup] = "(setup" ~> number ~ number ~ rep(name) <~ ")" ^^ { case humans ~ computers ~ players => Setup(humans, computers, players.toVector) }

  def game: Parser[Game] = "(game" ~> rep(state) <~ ")" ^^ { case states => Game(states.toVector) }

  // CHOICE
  def games: Parser[Games] = "(games)" ^^ (s => Games(""))

  def host: Parser[Host] = "(host" ~> setup <~ ")" ^^ { case setup => Host("", setup) }

  def join: Parser[Join] = "(join" ~> setup <~ ")" ^^ { case setup => Join("", setup) }

  def choice: Parser[Choice] = games | host | join

  // CLOSE
  def close: Parser[Close] = "close" ^^ (s => Close())

  // NUMBER AND NAME
  def number: Parser[Int] =
    """[0-9]+""".r ^^ (_.toInt)

  def name: Parser[String] = """[A-Za-z0-9]+""".r ^^ (_.toString)

  // CARDS
  def action: Parser[Action] =
    "cellar" ^^ (s => Cellar) | "festival" ^^ (s => Festival) | "laboratory" ^^ (s => Laboratory) |
      "market" ^^ (s => Market) | "mine" ^^ (s => Mine) | "remodel" ^^ (s => Remodel) |
      "smithy" ^^ (s => Smithy) | "village" ^^ (s => Village) | "woodcutter" ^^ (s => Woodcutter) |
      "workshop" ^^ (s => Workshop)

  def treasure: Parser[Treasure] =
    "copper" ^^ (s => Copper) | "silver" ^^ (s => Silver) | "gold" ^^ (s => Gold)

  def victory: Parser[Victory] =
    "estate" ^^ (s => Estate) | "duchy" ^^ (s => Duchy) | "province" ^^ (s => Province)

  def card: Parser[Card] = action | treasure | victory

  // PLAYS
  def act: Parser[Act] =
    "(act" ~> action ~ rep(card) <~ ")" ^^ { case action ~ cards => Act("", action, cards.toVector) }

  def add: Parser[Add] =
    "(add" ~> treasure <~ ")" ^^ { case treasure => Add("", treasure) }

  def buy: Parser[Buy] =
    "(buy" ~> card <~ ")" ^^ { case card => Buy("", card) }

  def clean: Parser[Clean] =
    "(clean" ~> card <~ ")" ^^ { case card => Clean("", Some(card)) } | "(clean)" ^^ (s => Clean("", None))

  def play: Parser[Play] = act | add | buy | clean | close

  // STATE
  def names: Parser[Vector[String]] =
    "(" ~> rep(name) <~ ")" ^^ { case names => names.toVector }

  def cards: Parser[Vector[Card]] =
    "(" ~> rep(card) <~ ")" ^^ { case cards => cards.toVector }

  def state: Parser[State] =
    "(state" ~> names ~ cards ~ cards ~ number ~ number ~ number ~ cards ~ cards ~ cards ~ cards <~ ")" ^^ { case players ~ supply ~ trash ~ actions ~ buys ~ coins ~ deck ~ hand ~ plays ~ discards => State(players, supply, trash, actions, buys, coins, deck, hand, plays, discards) }

  // NOTIFICATIONS
  def choose: Parser[Choose] =
    "(choose " ~> rep(setup) <~ ")" ^^ { case setups => Choose(setups.toVector) }

  def move: Parser[Move] =
    "(move" ~> state <~ ")" ^^ { case state => Move(state) }

  def moved: Parser[Moved] =
    "(moved" ~> name ~ play <~ ")" ^^ { case name ~ play => Moved(name, play) }

  def notification: Parser[Notification] = move | moved | close
}