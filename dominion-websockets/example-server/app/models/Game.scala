package models

case class Setup(humans: Int, computers: Int, players: Vector[String]) {
  override def toString =
    "(setup " + humans + " " + computers + " " + players.mkString(" ") + ")"
}

case class Game(states: Vector[State]) {
  override def toString =
    "(game " + states.mkString(" ") + ")"
}
