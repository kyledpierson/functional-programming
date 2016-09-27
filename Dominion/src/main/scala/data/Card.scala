package data

trait Card {
  val cost: Int
}

// --------------------------------------------------------------------- //
// ----------------------------- Treasure ------------------------------ //
// --------------------------------------------------------------------- //

trait Treasure extends Card {
  val value: Int
}

case object Copper extends Treasure {
  override val cost = 0
  override val value = 1

  override def toString = "copper"
}

case object Silver extends Treasure {
  override val cost = 3
  override val value = 2

  override def toString = "silver"
}

case object Gold extends Treasure {
  override val cost = 6
  override val value = 3

  override def toString = "gold"
}

// --------------------------------------------------------------------- //
// ------------------------------ Victory ------------------------------ //
// --------------------------------------------------------------------- //

trait Victory extends Card {
  val points: Int
}

case object Estate extends Victory {
  override val cost = 2
  override val points = 1

  override def toString = "estate"
}

case object Duchy extends Victory {
  override val cost = 5
  override val points = 3

  override def toString = "duchy"
}

case object Province extends Victory {
  override val cost = 8
  override val points = 6

  override def toString = "province"
}

// --------------------------------------------------------------------- //
// ------------------------------ Actions ------------------------------ //
// --------------------------------------------------------------------- //

trait Action extends Card {
  val benefit: Option[Benefit]
}

case class Benefit(cards: Int, actions: Int, buys: Int, coins: Int)

case object Cellar extends Action {
  override val cost = 2
  override val benefit = None

  override def toString = "cellar"
}

case object Festival extends Action {
  override val cost = 5
  override val benefit = Some(Benefit(0, 2, 1, 2))

  override def toString = "festival"
}

case object Laboratory extends Action {
  override val cost = 5
  override val benefit = Some(Benefit(2, 1, 0, 0))

  override def toString = "laboratory"
}

case object Market extends Action {
  override val cost = 5
  override val benefit = Some(Benefit(1, 1, 1, 1))

  override def toString = "market"
}

case object Militia extends Action {
  override val cost = 4
  override val benefit = Some(Benefit(0, 0, 0, 2))

  override def toString = "militia"
}

case object Mine extends Action {
  override val cost = 5
  override val benefit = None

  override def toString = "mine"
}

case object Moat extends Action {
  override val cost = 2
  override val benefit = Some(Benefit(2, 0, 0, 0))

  override def toString = "moat"
}

case object Remodel extends Action {
  override val cost = 4
  override val benefit = None

  override def toString = "remodel"
}

case object Smithy extends Action {
  override val cost = 4
  override val benefit = Some(Benefit(3, 0, 0, 0))

  override def toString = "smithy"
}

case object Village extends Action {
  override val cost = 3
  override val benefit = Some(Benefit(1, 2, 0, 0))

  override def toString = "village"
}

case object Woodcutter extends Action {
  override val cost = 3
  override val benefit = Some(Benefit(0, 0, 1, 2))

  override def toString = "woodcutter"
}

case object Workshop extends Action {
  override val cost = 3
  override val benefit = None

  override def toString = "workshop"
}