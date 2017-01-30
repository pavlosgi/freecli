package freecli
package examples
package option

import freecli.core.all._
import freecli.option.all._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    port: Int,
    host: String,
    user: String,
    password: String,
    database: String)

  val databaseConfig =
    group[DatabaseConfig] {
      int     --"port" -'p' -~ or(5432) -~ des("Database port") ::
      string  --"host"     -~ req -~ des("Database host")      ::
      string  --"username" -~ req -~ des("Database user")      ::
      string  --"password" -~ req -~ des("Database password")  ::
      string  --"database" -~ req -~ des("Database name") ::
      help    --"help" -~ des("Displays help") ::
      version --"version" -~ des("Displays version") -~ value("v1.0")
    }

  val res = runOptionOrFail(databaseConfig)(args)
  println(res)
}
