package models

trait Notification

case class Choose(setups: Vector[Setup]) extends Notification {
  override def toString = "(choose " + setups.mkString(" ") + ")"
}

case class Move(state: State) extends Notification {
  override def toString = "(move " + state + ")"
}

case class Moved(name: String, play: Play) extends Notification {
  override def toString = "(moved " + name + " " + play + ")"
}

case class Close() extends Notification with Play