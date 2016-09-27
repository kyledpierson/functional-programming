package data

trait Notification

case class Move(state: State) extends Notification {
  override def toString = "(move " + state + ")"
}

case class Moved(name: String, play: Play) extends Notification {
  override def toString = "(moved " + name + " " + play + ")"
}

case class Attacked(attack: Act, name: String, state: State) extends Notification {
  override def toString = "(attacked " + attack + " " + name + " " + state + ")"
}

case class Defended(name: String, defense: Defense) extends Notification {
  override def toString = "(defended " + name + " " + defense + ")"
}

case class Close() extends Notification with Play with Defense