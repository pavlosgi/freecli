package pavlosgi.freecli.examples.options

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.option.all._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    port: Int,
    host: String,
    user: String,
    password: String,
    database: String)

  val databaseConfig =
    group[DatabaseConfig] {
      int    --"port" -'p' -~ or(5432) -~ des("Database port") ::
      string -- "host"     -~ req -~ des("Database host")      ::
      string -- "username" -~ req -~ des("Database user")      ::
      string -- "password" -~ req -~ des("Database password")  ::
      string -- "database" -~ req -~ des("Database name")
    }

  val res = parseOptionsOrHelp(args)(databaseConfig)
  println(res)
}
