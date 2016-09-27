package data

trait Defense

case class Defend(defense: Action) extends Defense {
  override def toString = "(" + defense + ")"
}

case class Discard(cards: Vector[Card]) extends Defense {
  override def toString = "(discard " + cards.mkString(" ") + ")"
}