package pavlosgi.freecli.examples.options

import pavlosgi.freecli.config._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    port: Int,
    host: String,
    user: String,
    password: String,
    database: String)

  val databaseConfig =
    group[DatabaseConfig] {
      o.int --"port" -'p'    -~ or(5432) -~ des("Database port") ::
      o.string -- "host"     -~ req -~ des("Database host")      ::
      o.string -- "username" -~ req -~ des("Database user")      ::
      o.string -- "password" -~ req -~ des("Database password")  ::
      o.string -- "database" -~ req -~ des("Database name")
    }

  val res = parseConfigOrHelp(args)(databaseConfig)
  println(res)
}
