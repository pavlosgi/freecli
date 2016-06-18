package pavlosgi
package freecli
package core

import algebra._
import dsl._

import java.io.File

import cats.Show
import cats.data.{Validated, ValidatedNel, Xor}
import cats.std.all._
import cats.syntax.all._

object parser {

  sealed trait ParsingError

  object ParsingError {
    implicit object showInstance extends Show[ParsingError] {
      override def show(f: ParsingError): String = {
        f match {
          case FieldMissing(field) =>
            s"${field.show} is missing"

          case InvalidValueType(field, value) =>
            s"File $value for field ${field.show} does not exist"

          case FileDoesNotExist(field, value) =>
            s"File $value for field ${field.show} does not exist"

          case InvalidSubcommands =>
            s"Invalid subcommands"
        }
      }
    }
  }

  case class FieldMissing(field: FieldName) extends ParsingError
  case class InvalidValueType(field: FieldName, value: String) extends ParsingError
  case class FileDoesNotExist(field: FieldName, value: String) extends ParsingError
  case object InvalidSubcommands extends ParsingError

  type Result[A] = ValidatedNel[ParsingError, A]

  def parse[A](args: Seq[String])(d: ConfigDsl[A]): Result[A] = {

    d.apply(new Config[Result] {

      def arg[A, B](
        field: FieldName,
        description: Description,
        f: B => A,
        default: Option[B]
      )(implicit ev: Parser[B], ev2: Show[B]): Result[A] = {

        (for {
          value <- findArgValue(args, field)
          v <- ev(value).toRightXor(InvalidValueType(field, value))
        } yield f(v)).toValidatedNel
      }

      def int[A](
        field: FieldName,
        description: Option[Description],
        f: Int => A,
        default: Option[Int]
      ): Result[A] = {
        (for {
          value <- findArgValue(args, field)
          int <- safeParseInt(field, value)
        } yield f(int)).toValidatedNel
      }

      def optInt[A](field: FieldName, description: Option[Description], f: Option[Int] => A): Result[A] = {
        (for {
          value <- Xor.Right(findOptArgValue(args, field))
          int <- value.traverseU(safeParseInt(field, _))
        } yield f(int)).toValidatedNel
      }

      def string[A](
         field: FieldName,
         description: Option[Description],
         f: String => A,
         default: Option[String]
      ): Result[A] = {
        (for {
          value <- findArgValue(args, field)
        } yield f(value)).toValidatedNel
      }

      def optString[A](field: FieldName, description: Option[Description], f: Option[String] => A): Result[A] = {
        Validated.Valid(f(findOptArgValue(args, field)))
      }

      def boolean[A](
        field: FieldName,
        description: Option[Description],
        f: Boolean => A,
        default: Option[Boolean]
      ): Result[A] = {
        val boolean =
          findArgValue(args, field).toOption.fold(
            Xor.right[ParsingError, Boolean](existsField(args, field))
          )(safeParseBoolean(field, _))

        boolean.toValidatedNel.map(f)
      }

      def file[A](
        field: FieldName,
        description: Option[Description],
        f: File => A,
        default: Option[File]
      ): Result[A] = {
        (for {
          value <- findArgValue(args, field)
          file = new File(value)
          _ <- if (file.exists)
                Xor.Right(value)
               else
                Xor.Left(FileDoesNotExist(field, value))

        } yield f(file)).toValidatedNel
      }

      def optFile[A](field: FieldName, description: Option[Description], f: Option[File] => A): Result[A] = {
        (for {
          value <- Xor.Right(findOptArgValue(args, field))
          optFile = value.map(new File(_))
          file <- optFile.traverseU { v =>
                      if (v.exists)
                        Xor.Right(v)
                      else
                        Xor.Left(FileDoesNotExist(field, v.getAbsolutePath))
                  }

        } yield f(file)).toValidatedNel
      }

      def sub[A](
        name: ConfigName,
        description: Option[Description],
        f: Result[A],
        default: Option[A]
      ): Result[A] = {
        f
      }

      def cmd[A](
        name: CommandName,
        description: Option[Description],
        config: Result[Option[A]],
        subcommands: Result[Commands],
        run: Option[A] => Unit
      ): Result[Command[A]] = {
        (config |@| subcommands).map {
          case (c, s) => Command(name, c, s, run)
        }
      }

      override def pure[A](x: A): Result[A] = Validated.Valid(x)

      override def ap[A, B](ff: Result[A => B])(fa: Result[A]): Result[B] =
        fa.ap(ff)

    })
  }

  def parseOrExit[A](args: Seq[String])(config: ConfigDsl[A]): A = {
    parse(args)(config) match {
      case Validated.Invalid(e) =>
        val errors =
          s"""
             |${help.genHelp(config)}
             |${"Errors".red.underline}
             |
             |${e.toList.map(er => er.show.indent(1)).mkString("\n")}""".stripMargin

        println(errors)
        sys.exit(1)

      case Validated.Valid(r) => r
    }
  }

  def safeParseInt(field: FieldName, value: String): Xor[ParsingError, Int] = {
    try {
      Xor.Right(value.toInt)
    } catch {
      case e: NumberFormatException =>
        Xor.Left(InvalidValueType(field, value))
    }
  }

  def safeParseBoolean(field: FieldName, value: String): Xor[ParsingError, Boolean] = {
    try {
      Xor.Right(value.toBoolean)
    } catch {
      case e: IllegalArgumentException =>
        Xor.Left(InvalidValueType(field, value))
    }
  }

  def findArgValue(args: Seq[String], field: FieldName): Xor[ParsingError, String] = {
    findOptArgValue(args, field).toRightXor(FieldMissing(field))
  }

  def findOptArgValue(args: Seq[String], field: FieldName): Option[String] = {
    val Pattern = s"${field.show}=(.*)".r

    args.collectFirst {
      case Pattern(v) if v.nonEmpty => v
    }
  }

  def existsField(args: Seq[String], field: FieldName): Boolean = {
    args.exists(_.matches(s"${field.show}"))
  }

}
