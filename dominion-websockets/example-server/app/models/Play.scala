package models

trait Play

case class Act(name: String, action: Action, cards: Vector[Card]) extends Play {
  override def toString = "(act " + action + " " + cards.mkString(" ") + ")"
}

case class Add(name: String, treasure: Treasure) extends Play {
  override def toString = "(add " + treasure + ")"
}

case class Buy(name: String, card: Card) extends Play {
  override def toString = "(buy " + card + ")"
}

case class Clean(name: String, card: Option[Card]) extends Play {
  override def toString = if (card.isDefined) "(clean " + card.get + ")" else "(clean)"
}
