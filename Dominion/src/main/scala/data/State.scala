package data

case class State(players: Vector[String],
                 supply: Vector[Card],
                 trash: Vector[Card],
                 actions: Int,
                 buys: Int,
                 coins: Int,
                 deck: Vector[Card],
                 hand: Vector[Card],
                 plays: Vector[Card],
                 discards: Vector[Card]) {

  override def toString = "(state (" +
    players.mkString(" ") + ") (" +
    supply.mkString(" ") + ") (" +
    trash.mkString(" ") + ") " +
    actions + " " +
    buys + " " +
    coins + " (" +
    deck.mkString(" ") + ") (" +
    hand.mkString(" ") + ") (" +
    plays.mkString(" ") + ") (" +
    discards.mkString(" ") + "))"
}
