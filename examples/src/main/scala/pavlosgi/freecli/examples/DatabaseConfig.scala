package pavlosgi.freecli.examples

import pavlosgi.freecli.config._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    host: String,
    port: Int,
    user: String,
    password: String,
    database: String)

  val databaseConfig =
    group[DatabaseConfig] {
      o.string --"host" -'h' -~ or("localhost") -~ des("Database host")  ::
      o.int    --"port" -'p' -~ or(5432) -~ des("Database port")         ::
      o.string --"username" -'u' -~ req -~ des("Database user")          ::
      o.string --"password" -~ req -~ des("Database password")           ::
      o.string --"database" -~ req -~ des("Database name")
    }

  parseConfigOrHelp(args)(databaseConfig)
}
