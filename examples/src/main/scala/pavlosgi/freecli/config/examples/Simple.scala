package pavlosgi
package freecli
package config
package examples

import all._

object Simple extends App {
  case class AuthConfig(port: Int, host: Option[String])
  case class ServerConfig(logging: Boolean, auth: AuthConfig)

  val authConfig =
    (int("port", None, Some("some port for some db"), Some(5432)) |@|
     optString("host", None, Some("some host"))).map(AuthConfig)

  val serverConfig =
    (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)
}

