package pavlosgi.freecli.commands.examples

import pavlosgi.freecli.all._

object Simple extends App {
  object Configurations {
    case class DatabaseConfig
      (user: String,
       password: String,
       database: String,
       host: String)

    case class CommonConfig[A](logging: Boolean, config: A)

    private val dbConfig =
      (string("user", None, Some("database user")) |@|
       string("password", None, Some("database password")) |@|
       string("database", None, Some("database name")) |@|
       string("host", None, Some("database host"))).map(DatabaseConfig)

    val databaseConfig =
      (boolean("logging", default = Some(true)) |@| sub("database")(dbConfig))
        .map(CommonConfig[DatabaseConfig])
  }

  object Commands {
    import Configurations._

    lazy val serverOps =
      cmd("server-ops") {
        subcommand(createSchema) ~
        subcommand(createTables) ~
        subcommand(createUsers) ~
        subcommand(createSequences)
      }

    val createSchema =
      cmd("create-schema") {
        config(Configurations.databaseConfig) ~
        run((c: Configurations.CommonConfig[DatabaseConfig]) => ())
      }

    val createTables =
      cmd("create-tables") {
        config(Configurations.databaseConfig) ~
        run((c: Configurations.CommonConfig[DatabaseConfig]) => ())
      }

    val createUsers =
      cmd("create-users") {
        config(Configurations.databaseConfig) ~
        run((c: Configurations.CommonConfig[DatabaseConfig]) => ())
      }

    val createSequences =
      cmd("create-sequences") {
        config(Configurations.databaseConfig) ~
        run((c: Configurations.CommonConfig[DatabaseConfig]) => ())
      }
  }

  println(parseOrExit(
    Seq("server-ops", "create-sequences", "database",
        "--user", "freecli", "--password", "password", "--database",
        "freecli", "--host", "localhost")

  )(Commands.serverOps))
}

