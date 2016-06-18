package pavlosgi
package freecli
package examples

import freecli.core.dsl._
import freecli.core.help._

import cats.syntax.cartesian._

object Simple extends App {

  case class AuthConfig(port: Int, host: Option[String])
  case class ServerConfig(logging: Boolean, auth: AuthConfig)

  val authConfig = (int("port", Some("some port for some db"), Some(5432)) |@| optString("host", Some("some host"))).map(AuthConfig)
  val serverConfig: ConfigDsl[ServerConfig] =
    (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)

  val cmd1 = cmd("CMD1").config(serverConfig).subcommand {
     cmd("CMD2").config(serverConfig)
   }.subcommand {
    cmd("CMD4").config(serverConfig)
  }

  val cmd3 = cmd("CMD3", Some("Some command")).config(serverConfig)

  val x = (
    serverConfig |@| cmd1 |@| cmd3
  ).map((_, a, b) => a.commands ++ b.commands)
//  println(genHelpCommands(x))

 // val c = parseCommandsOrExit(args)(x)
 println(genHelp(x))
}

