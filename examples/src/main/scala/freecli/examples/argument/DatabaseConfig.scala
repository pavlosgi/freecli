package freecli
package examples
package argument

import freecli.argument.all._
import freecli.core.all._

object DatabaseConfig extends App {
  case class DatabaseConfig(
    port: Int,
    host: String,
    user: String,
    password: String,
    database: String
  )

  val databaseConfig =
    group[DatabaseConfig] {
      int -~ name("port") -~ des("Database port") ::
      string -~ name("host") -~ des("Database host") ::
      string -~ name("username") -~ des("Database user") ::
      string -~ name("password") -~ des("Database password") ::
      string -~ name("database") -~ des("Database name")
    }

  val res = runArgumentOrFail(databaseConfig)(args)
  println(res)
}
