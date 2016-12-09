package pavlosgi.freecli.examples.config

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
      o.int --"port" -'p' -~ or(5432) -~ des("Database port") ::
      string -~ name("host")     -~ des("Database host")      ::
      string -~ name("username") -~ des("Database user")      ::
      string -~ name("password") -~ des("Database password")  ::
      string -~ name("database") -~ des("Database name")
    }

  val res = parseConfigOrHelp(args)(databaseConfig)
  println(res)
}
