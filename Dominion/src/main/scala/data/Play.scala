package data

trait Play

case class Act(action: Action, cards: Vector[Card]) extends Play {
  override def toString = "(act " + action + " " + cards.mkString(" ") + ")"
}

case class Add(treasure: Treasure) extends Play {
  override def toString = "(add " + treasure + ")"
}

case class Buy(card: Card) extends Play {
  override def toString = "(buy " + card + ")"
}

case class Clean(card: Option[Card]) extends Play {
  override def toString = if (card.isDefined) "(clean " + card.get + ")" else "(clean)"
}
