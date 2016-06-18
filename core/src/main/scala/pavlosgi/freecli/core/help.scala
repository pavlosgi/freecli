package pavlosgi
package freecli
package core

import algebra._
import dsl._

import java.io.File

import cats.Show
import cats.data.State
import cats.std.all._
import cats.syntax.show._

object help {

  case class ConfigHelp(
    args: Seq[String] = Seq.empty,
    opts: Seq[String] = Seq.empty,
    subConfigs: Seq[(String, ConfigHelp)] = Seq.empty,
    commands: Seq[(String, ConfigHelp)] = Seq.empty
  ){
    def asString(indentation: Int): String = {
      val argsAsString = args.map(_.bold.indent(indentation)).mkString("\n")
      val optsAsString = opts.map(_.indent(indentation)).mkString(s"\n")
      val subConfigsAsString = subConfigs.map { c =>
        c._1.cyan.indent(indentation) + "\n" + c._2.asString(indentation + 1)
      }.mkString("\n\n")

      val commandsAsString = commands.map { c =>
        c._1.yellow.indent(indentation) + "\n" + c._2.asString(indentation + 1)
      }.mkString("\n\n")

      val all = Seq(Seq(argsAsString, optsAsString, subConfigsAsString).mkString("\n"), commandsAsString)
      all.filter(_.nonEmpty).mkString("\n")
    }

    def usage = s"${"Usage".underline}\n\n${asString(1)}"
  }

  type ConfigPrinter[_] = State[ConfigHelp, Unit]

  def genHelp[A](p: ConfigDsl[A]): String = {
    val help = p.apply(new Config[ConfigPrinter] {
      override def pure[A](x: A): ConfigPrinter[A] = State.pure(())

      override def ap[A, B](ff: ConfigPrinter[(A) => B])(fa: ConfigPrinter[A]): ConfigPrinter[B] = {
        for {
          h1 <- ff.get
          h2 <- fa.get
        } yield State.pure(ConfigHelp(h1.args ++ h2.args, h1.opts ++ h2.opts, h1.subConfigs ++ h2.subConfigs))
      }

      def arg[A, B](
        field: FieldName,
        description: Description,
        value: B => A,
        default: Option[B]
      )(implicit ev: Parser[B], ev2: Show[B]): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.args :+
            s"${field.show} - ${descriptionAndDefault(Some(description), default)}"))

        } yield ()
      }

      def int[A](
        field: FieldName,
        description: Option[Description],
        value: Int => A,
        default: Option[Int]
      ): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.args :+
            s"${field.show} - an integer ${descriptionAndDefault(description, default)}"))

        } yield ()
      }

      def optInt[A](field: FieldName, description: Option[Description], value: Option[Int] => A): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(opts = h.opts :+
            s"${field.show} - an optional integer ${description.orEmpty.parenthesis}"))

        } yield ()
      }

      def string[A](
         field: FieldName,
         description: Option[Description],
         value: String => A,
         default: Option[String]
      ): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.args :+
            s"${field.show} - a string ${descriptionAndDefault(description, default)}"))

        } yield ()
      }

      def optString[A](
        field: FieldName,
        description: Option[Description],
        value: Option[String] => A
      ): ConfigPrinter[A] = {

        for {
          _ <- State.modify[ConfigHelp](h => h.copy(opts = h.opts :+
            s"${field.show} - an optional string ${description.orEmpty.parenthesis}"))

        } yield ()
      }

      def boolean[A](
        field: FieldName,
        description: Option[Description],
        value: Boolean => A,
        default: Option[Boolean]
      ): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.opts :+
            s"${field.show} - a boolean flag (default = false) ${descriptionAndDefault(description, default)}"))

        } yield ()
      }

      def file[A](
        field: FieldName,
        description: Option[Description],
        value: File => A,
        default: Option[File]
      ): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.args :+
            s"${field.show} - a file path ${descriptionAndDefault(description, default.map(_.getAbsolutePath))}"))

        } yield ()
      }

      def optFile[A](field: FieldName, description: Option[Description], value: Option[File] => A): ConfigPrinter[A] = {
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(opts = h.opts :+
            s"${field.show} - an optional file path ${description.orEmpty.parenthesis}"))

        } yield ()
      }
      def sub[A](
        name: ConfigName,
        description: Option[Description],
        value: ConfigPrinter[A],
        default: Option[A]
      ): ConfigPrinter[A] = {

        val subState = value.runS(ConfigHelp()).value
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(subConfigs = h.subConfigs :+
                 s"${name.show} - a sub-configuration ${descriptionAndDefault(description, default.map(_.toString))}" ->
                 subState
               ))

        } yield ()
      }

      def cmd[A](
        name: CommandName,
        description: Option[Description],
        config: ConfigPrinter[Option[A]],
        subcommands: ConfigPrinter[List[Command[_]]],
        run: Option[A] => Unit
      ): ConfigPrinter[Command[A]] = {

        val cmdHelp = config.runS(ConfigHelp()).value
        val subcommandsHelp = subcommands.runS(ConfigHelp()).value
        val allHelp = cmdHelp.copy(
          args = cmdHelp.args ++ subcommandsHelp.args,
          opts = cmdHelp.opts ++ subcommandsHelp.opts,
          subConfigs = cmdHelp.subConfigs ++ subcommandsHelp.subConfigs,
          commands = cmdHelp.commands ++ subcommandsHelp.commands)

        for {
          _ <- State.modify[ConfigHelp](h => h.copy(commands = h.commands :+
            s"${name.show} - a command ${description.orEmpty.parenthesis}" -> allHelp
          ))

        } yield ()
      }

    }).runS(ConfigHelp()).value

    help.usage
  }

  def descriptionAndDefault[A](description: Option[Description], default: Option[A])(implicit ev: Show[A]): String = {
    (description, default) match {
      case (None, None) => ""
      case (Some(d), None) => d.show.parenthesis
      case (None, Some(d)) => s"(default = ${ev.show(d)})"
      case (Some(d), Some(de)) => s"(${d.show}, default = ${ev.show(de)})"
    }
  }
}
