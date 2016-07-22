package pavlosgi
package freecli
package commands
package parser

import algebra._
import dsl.CommandsDsl
import pavlosgi.freecli.config.algebra.{ApplyConfigAlgebra, Plugin}
import pavlosgi.freecli.config.{parser => ConfigParser}
import pavlosgi.freecli.config.parser.{Operations => ConfigParserOps}

import cats.data._
import cats.std.list._
import cats.syntax.all._
import cats.~>

trait Operations extends {
  type StateResult[A] = State[Seq[String], A]
  type Result[A] = XorT[StateResult, NonEmptyList[ParsingError], A]

  def parse[G[_]: Plugin]
           (args: Seq[String])
           (d: CommandsDsl[G, Command])
           (implicit nat: G ~> ConfigParser.Parser): ValidatedNel[ParsingError, Command] = {

    parseAndReturnExtraArgs(args)(d) match {
      case (Nil, r)     => r.toValidated
      case (remArgs, r) =>
        r.toValidated.leftMap(e => NonEmptyList(InvalidArgs(remArgs), e.toList))
    }
  }

  private def parseAndReturnExtraArgs[G[_]: Plugin, A]
    (args: Seq[String])
    (d: CommandsDsl[G, Command])
    (implicit nat: G ~> ConfigParser.Parser):
    (Seq[String], NonEmptyList[ParsingError] Xor Command) = {

    d.apply(parserAlgebra).value.run(args).value match {
      case (remArgs, r) => remArgs -> r
    }
  }

  def parserAlgebra[G[_]: Plugin]
                   (implicit nat: G ~> ConfigParser.Parser):
                   CommandAlgebra[Result, G] =

    new CommandAlgebra[Result, G] {
      override def cmd(field: CommandField,
                       runF: => Unit,
                       f: ApplyCommandAlgebra[G, Command]): Result[Command] = {
        for {
          args <- findCommandArgs(field)
          (cmdRemArgs, cmdResX) =
            if (args.isEmpty)
              (args, Xor.right(new Command {
                def run: Unit = runF
              }))

            else parseAndReturnExtraArgs(args)(f)

          cmdRes <- XorT.fromXor[StateResult](cmdResX)
          _   <- XorT.right(State.modify[Seq[String]](s => s ++ cmdRemArgs))

        } yield cmdRes
      }

      override def cmdWithConfig[A](field: CommandField,
                                    config: ApplyConfigAlgebra[G, A],
                                    runF: A => Unit,
                                    f: ApplyCommandAlgebra[G, Command]):
                                    Result[Command] = {
        for {
          args <- findCommandArgs(field)
          (configRemArgs, configResX) =
            ConfigParserOps.parseConfigAndReturnExtraArgs(args)(config)

          configRes <- XorT.fromXor[StateResult](configResX.leftMap(toCommandError))

          (cmdRemArgs, cmdResX) =
            if (configRemArgs.isEmpty)
              (Seq.empty, Xor.right(new Command {
                def run: Unit = runF(configRes)
              }))

            else parseAndReturnExtraArgs(configRemArgs)(f)

          cmdRes <- XorT.fromXor[StateResult](cmdResX)
          _   <- XorT.right(State.modify[Seq[String]](s => s ++ cmdRemArgs))

        } yield cmdRes
      }

      override def ap[A, B](ff: Result[(A) => B])(fa: Result[A]): Result[B] = {
        XorT.apply(for {
          faXor <- fa.value
          ffXor <- ff.value
        } yield {
          faXor.toValidated.ap(ffXor.toValidated).toXor
        })
      }

      override def empty[A]: Result[A] =
        XorT.left(State.pure(NonEmptyList(CommandNotMatched)))

      override def combineK[A](x: Result[A], y: Result[A]): Result[A] = {
        XorT.apply(for {
          xS <- x.value
          yS <- y.value
        } yield {
          (xS, yS) match {
            case (r@Xor.Right(v), _) => r
            case (_, r@Xor.Right(v)) => r
            case (Xor.Left(e1), Xor.Left(e2)) => Xor.Left(e1.combine(e2))
          }
        })
      }

      override def pure[A](x: A): Result[A] = XorT.pure(x)
    }

  def findCommandArgs(field: CommandField): Result[Seq[String]] = {
    def findArgs(args: Seq[String]):
      (Seq[String], NonEmptyList[ParsingError] Xor Seq[String]) = {

        val Pattern = s"^${field.name.show}$$".r
        val index = args.indexWhere(Pattern.pattern.matcher(_).matches)
        if (index != -1) {
          args.take(index) -> Xor.right(args.drop(index + 1))
        } else {
          args -> Xor.left(NonEmptyList[ParsingError](CommandNotFound(field)))
        }
    }

    XorT.apply(for {
      args <- State.get[Seq[String]]
      (remArgs, commandArgs) = findArgs(args)
      res <- State.pure(commandArgs)
      _   <- State.set(remArgs)
    } yield res)
  }

  def toCommandError(nel: NonEmptyList[ConfigParser.ParsingError]):
                     NonEmptyList[ParsingError] = {

    nel.map(ConfigError)
  }

}

object Operations extends Operations
