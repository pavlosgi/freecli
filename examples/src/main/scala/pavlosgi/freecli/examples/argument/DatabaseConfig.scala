package pavlosgi.freecli.examples.argument

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.argument.all._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    port: Int,
    host: String,
    user: String,
    password: String,
    database: String)

  val databaseConfig =
    group[DatabaseConfig] {
      int    -~ name("port")     -~ des("Database port")      ::
      string -~ name("host")     -~ des("Database host")      ::
      string -~ name("username") -~ des("Database user")      ::
      string -~ name("password") -~ des("Database password")  ::
      string -~ name("database") -~ des("Database name")
    }

  val res = runArgumentOrFail(databaseConfig)(args)
  println(res)
}
