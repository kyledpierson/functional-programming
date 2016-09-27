package models

object DominionValidator {
  def is_valid(state: State, play: Play): Boolean = {

    play match {
      case Act(name, action, cards) =>
        if (state.actions < 1) return false
        action match {
          case Cellar =>
            val new_hand = state.hand.diff(cards)
            new_hand.contains(Cellar) && cards.length == state.hand.length - new_hand.length
          case Mine =>
            cards.length == 2 &&
              state.hand.contains(Mine) && state.hand.contains(cards.head) && state.supply.contains(cards(1)) &&
              cards.head.isInstanceOf[Treasure] && cards(1).isInstanceOf[Treasure] && cards(1).cost <= cards.head.cost + 3
          case Remodel =>
            if (state.hand.contains(Remodel) && cards.length == 2) {
              val new_hand = state.hand.diff(Vector(Remodel))
              new_hand.contains(cards.head) && state.supply.contains(cards(1)) && cards(1).cost <= cards.head.cost + 2
            } else false
          case Workshop =>
            cards.length == 1 &&
              state.hand.contains(Workshop) && state.supply.contains(cards.head) && cards.head.cost <= 4
          case _ => state.hand.contains(`action`)
        }
      case Add(name, treasure) =>
        state.hand.contains(`treasure`)
      case Buy(name, card) =>
        state.supply.contains(`card`) && state.buys > 0 && card.cost <= state.coins
      case Clean(name, card) =>
        (card.isDefined && state.hand.contains(card.get)) || card.isEmpty
    }
  }
}
