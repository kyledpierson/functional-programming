package config

object Routes {

  object Dominion {
    val base = "/dominion"
    def connectSSE(username: String) = base + s"/sse/$username"
    def talk = base + "/talk"
  }
}
