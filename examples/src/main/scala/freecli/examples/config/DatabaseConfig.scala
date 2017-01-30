package freecli
package examples
package config

import freecli.core.all._
import freecli.config.all._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    port: Int,
    debug: Boolean,
    verbose: Boolean,
    host: String,
    user: String,
    password: String,
    database: String)

  val databaseConfig =
    group[DatabaseConfig] {
      O.int --"port" -'p' -~ or(5432) -~ des("Database port") ::
      flag -'d' -~ des("Debug mode")                          ::
      flag -'v' -~ des("Verbose mode")                        ::
      string -~ name("host")     -~ des("Database host")      ::
      string -~ name("username") -~ des("Database user")      ::
      string -~ name("password") -~ des("Database password")  ::
      string -~ name("database") -~ des("Database name")
    }

  val res = runConfigOrFail(databaseConfig)(args)
  println(res)
}
