package pavlosgi
package freecli
package core

import java.io.File

import cats.{Applicative, Monoid, Show}

object algebra {
  trait Config[F[_]] extends Applicative[F] {
    def arg[A, B](
      field: FieldName, 
      description: Description, 
      f: B => A, 
      default: Option[B]
    )(implicit ev: Parser[B], ev2: Show[B]): F[A]

    def int[A](
      field: FieldName,
      description: Option[Description],
      f: Int => A,
      default: Option[Int]
    ): F[A]

    def optInt[A](field: FieldName, description: Option[Description], f: Option[Int] => A): F[A]
    def string[A](
       field: FieldName,
       description: Option[Description],
       f: String => A,
       default: Option[String]
    ): F[A]

    def optString[A](field: FieldName, description: Option[Description], f: Option[String] => A): F[A]
    def boolean[A](
      field: FieldName,
      description: Option[Description],
      f: Boolean => A,
      default: Option[Boolean]
    ): F[A]

    def file[A](
      field: FieldName,
      description: Option[Description],
      f: File => A,
      default: Option[File]
    ): F[A]

    def optFile[A](field: FieldName, description: Option[Description], f: Option[File] => A): F[A]
    def sub[A](name: ConfigName, description: Option[Description], f: F[A], default: Option[A]): F[A]
    def cmd[A](
      name: CommandName,
      description: Option[Description],
      config: F[Option[A]], 
      subcommands: F[Commands],
      run: Option[A] => Unit
    ): F[Command[A]]
  }

  class FieldName private(val name: String)
  object FieldName {
    def apply(v: String): FieldName = {
      val fname = v.replaceAll(" ", "-").replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
      new FieldName(fname)
    }

    implicit object showInstance extends Show[FieldName] {
      override def show(f: FieldName): String = s"--${f.name}"
    }
  }

  case class Description(value: String)
  object Description {
    implicit object showInstance extends Show[Description] {
      override def show(f: Description): String = s"${f.value}"
    }
  }

  case class ConfigName(value: String)
  object ConfigName {
    implicit object showInstance extends Show[ConfigName] {
      override def show(c: ConfigName): String = s"${c.value}"
    }
  }

  case class CommandName(value: String)
  object CommandName {
    implicit object showInstance extends Show[CommandName] {
      override def show(c: CommandName): String = s"${c.value}"
    }
  }

  case class Command[A](
    name: CommandName, 
    config: Option[A], 
    subcommands: Commands,
    run: Option[A] => Unit
  )
  
  object Command {
    implicit def c2Commands[A](c: Command[A]): Commands = Commands.apply(c)
  }

  case class Commands(commands: List[Command[_]])
  object Commands {
    def apply[A](c: Command[A]): Commands = Commands(List(c))

    implicit object monoidInstance extends Monoid[Commands] {
      def empty: Commands = Commands(List.empty)
      def combine(x: Commands,y: Commands): Commands = 
        Commands(x.commands ++ y.commands)
    }
  }

  trait Parser[T] {
    def apply(v: String): Option[T]
  }

  abstract class ParserF[F, T](implicit ev: Parser[F]) extends Parser[T] {
    def from(v: F): Option[T]
    def apply(v: String): Option[T] = ev(v).flatMap(from)
  }

  object Parser extends ParserInstances

  trait ParserInstances {
    implicit def strParser = new Parser[String] {
      override def apply(v: String): Option[String] = Some(v)
    }

    implicit def fileParser = new Parser[File] {
      override def apply(v: String): Option[File] = Some(new File(v))
    }

    case class F(v: File)
    implicit def fparser = new ParserF[File, F] {
      override def from(v: File): Option[F] = Some(F(v))
    }

    case class D(v: F)
    implicit def dparser = new ParserF[F, D] {
      override def from(v: F): Option[D] = Some(D(v))
    }

  }

}
