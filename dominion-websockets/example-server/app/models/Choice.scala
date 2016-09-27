package models

import play.api.libs.json.JsValue
import play.api.libs.iteratee.Enumerator

sealed trait Choice

case class Host(name: String, setup: Setup) extends Choice {
  override def toString = "(host " + setup + ")"
}

case class Join(name: String, setup: Setup) extends Choice {
  override def toString = "(join " + setup + ")"
}

case class Games(name: String) extends Choice {
  override def toString = "(games)"
}

case class Connect(name: String)

case class Connected(name: String, enumerator: Enumerator[JsValue])

case class CannotConnect(msg: String)