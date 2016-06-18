package pavlosgi
package freecli
package core

import algebra._

import java.io.File

import cats.std.all._
import cats.syntax.all._
import cats.{Applicative, Show}

object dsl {

  sealed trait ConfigDsl[A] {
    def apply[F[_]: Config]: F[A]
  }

  object ConfigDsl {
    implicit def toConfigDslCommands[A](c: ConfigDsl[Command[A]]): ConfigDsl[Commands] = {
      new ConfigDsl[Commands] {
        def apply[F[_]: Config]: F[Commands] = {
          c.apply[F].map(Commands.apply(_))
        }
      }
    }
  }

  def arg[B](field: String, description: String, default: Option[B] = None)
    (implicit ev: Parser[B], ev2: Show[B]): ConfigDsl[B] = {

      new ConfigDsl[B] {
        def apply[F[_]: Config] =
          implicitly[Config[F]].arg(FieldName(field), Description(description), (b: B) => b, default)
      }
  }

  def int(field: String, description: Option[String] = None, default: Option[Int] = None): ConfigDsl[Int] = {
    new ConfigDsl[Int] {
      def apply[F[_] : Config] = implicitly[Config[F]].int(
                                  FieldName(field),
                                  description.map(Description.apply),
                                  identity,
                                  default)
    }
  }

  def optInt(field: String, description: Option[String] = None): ConfigDsl[Option[Int]] =
    new ConfigDsl[Option[Int]] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].optInt(FieldName(field), description.map(Description.apply), identity)
    }

  def string(field: String, description: Option[String] = None, default: Option[String] = None): ConfigDsl[String] =
    new ConfigDsl[String] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].string(FieldName(field), description.map(Description.apply), identity, default)
    }

  def optString(field: String, description: Option[String] = None): ConfigDsl[Option[String]] =
    new ConfigDsl[Option[String]] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].optString(FieldName(field), description.map(Description.apply), identity)
    }

  def boolean(field: String, description: Option[String] = None, default: Option[Boolean] = None): ConfigDsl[Boolean] =
    new ConfigDsl[Boolean] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].boolean(FieldName(field), description.map(Description.apply), identity, default)
    }

  def file(field: String, description: Option[String] = None, default: Option[File] = None): ConfigDsl[File] =
    new ConfigDsl[File] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].file(FieldName(field), description.map(Description.apply), identity, default)
    }

  def optFile(field: String, description: Option[String] = None): ConfigDsl[Option[File]] =
    new ConfigDsl[Option[File]] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].optFile(FieldName(field), description.map(Description.apply), identity)
    }

  def sub[A](name: String)
            (dsl: ConfigDsl[A], description: Option[String] = None, default: Option[A] = None)
            :ConfigDsl[A] = {

    new ConfigDsl[A] {
      def apply[F[_]: Config] =
        implicitly[Config[F]].sub(ConfigName(name), description.map(Description.apply), dsl.apply[F], default)
    }
  }

  def cmd(name: String, description: Option[String] = None): CommandDsl = {
    new CommandDsl(CommandName(name), description.map(Description.apply))
  }

  class CommandDsl(
    private[core] val name: CommandName,
    private[core] val description: Option[Description],
    private[core] val subcommands: List[ConfigDsl[Commands]] = List.empty,
    private[core] val run: Unit = ()
  )

  object CommandDsl {
    implicit def toConfigDsl[A](c: CommandDsl): ConfigDsl[Command[A]] = {
      new ConfigDsl[Command[A]] {
        def apply[F[_]: Config]: F[Command[A]] = {
          implicitly[Config[F]].cmd(
            c.name,
            c.description,
            Option.empty.pure[F],
            c.subcommands.traverseU(s => s.apply[F]).map(_.combineAll),
            (x: Option[A]) => c.run
          )
        }
      }
    }
  }

  class CommandDslWithConfig[A](
    private[core] val name: CommandName,
    private[core] val description: Option[Description],
    private[core] val config: ConfigDsl[A],
    private[core] val subcommands: List[ConfigDsl[Commands]] = List.empty,
    private[core] val run: A => Unit = (a: A) => ())

  object CommandDslWithConfig {
    implicit def toConfigDsl[A](c: CommandDslWithConfig[A]): ConfigDsl[Command[A]] = {
      new ConfigDsl[Command[A]] {
        def apply[F[_]: Config]: F[Command[A]] = {
          implicitly[Config[F]].cmd(
            c.name,
            c.description,
            c.config.apply[F].map(Some.apply),
            c.subcommands.traverseU(s => s.apply[F]).map(_.combineAll),
            (x: Option[A]) => x.fold(())(c.run)
          )
        }
      }
    }
  }

  implicit class CommandDslOps(dsl: CommandDsl) {
    def config[A](c: ConfigDsl[A]): CommandDslWithConfig[A] = {
      new CommandDslWithConfig(dsl.name, dsl.description, c, dsl.subcommands)
    }

    def subcommand[B](s: ConfigDsl[Command[B]]): CommandDsl = {
      new CommandDsl(dsl.name, dsl.description, dsl.subcommands :+ ConfigDsl.toConfigDslCommands(s), dsl.run)
    }

    def run(r: Unit): CommandDsl = {
      new CommandDsl(dsl.name, dsl.description, dsl.subcommands, dsl.run)
    }
  }

  implicit class CommandDslWithConfigOps[A](dsl: CommandDslWithConfig[A]) {
    def subcommand[B](s: ConfigDsl[Command[B]]): CommandDslWithConfig[A] = {
      new CommandDslWithConfig(
        dsl.name,
        dsl.description,
        dsl.config,
        dsl.subcommands :+ ConfigDsl.toConfigDslCommands(s),
        dsl.run)
    }

    def run(r: A => Unit): CommandDslWithConfig[A] = {
      new CommandDslWithConfig(dsl.name, dsl.description, dsl.config, dsl.subcommands, dsl.run)
    }
  }

  implicit val ApplicativeDsl: Applicative[ConfigDsl] = new Applicative[ConfigDsl] {
    override def pure[A](x: A): ConfigDsl[A] = new ConfigDsl[A] { def apply[F[_]: Config] = x.pure[F]}

    override def ap[A, B](ff: ConfigDsl[(A) => B])(fa: ConfigDsl[A]): ConfigDsl[B] = new ConfigDsl[B] {
      override def apply[F[_] : Config]: F[B] = ff.apply[F].ap(fa.apply[F])
    }
  }
}
