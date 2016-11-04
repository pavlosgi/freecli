package pavlosgi
package freecli
package commands
package parser

import algebra._
import dsl.CommandsDsl
import pavlosgi.freecli.config.algebra.{ApplyConfigAlgebra, Plugin}
import pavlosgi.freecli.config.parser.{all => ConfigParser}

import cats.data._
import cats.std.list._
import cats.syntax.all._
import cats.~>

trait Operations extends {
  type StateResult[A] = State[Seq[String], A]
  type Result[A] = EitherT[StateResult, NonEmptyList[ParsingError], A]

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
    (Seq[String], NonEmptyList[ParsingError] Either Command) = {

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
              (args, Right(new Command {
                def run: Unit = runF
              }))

            else parseAndReturnExtraArgs(args)(f)

          cmdRes <- EitherT.fromEither[StateResult](cmdResX)
          _   <- EitherT.right(State.modify[Seq[String]](s => s ++ cmdRemArgs))

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
            ConfigParser.parseAndReturnExtraArgs(args)(config)

          configRes <- EitherT.fromEither[StateResult](configResX.leftMap(toCommandError))

          (cmdRemArgs, cmdResX) =
            if (configRemArgs.isEmpty)
              (Seq.empty, Right(new Command {
                def run: Unit = runF(configRes)
              }))

            else parseAndReturnExtraArgs(configRemArgs)(f)

          cmdRes <- EitherT.fromEither[StateResult](cmdResX)
          _   <- EitherT.right(State.modify[Seq[String]](s => s ++ cmdRemArgs))

        } yield cmdRes
      }

      override def ap[A, B](ff: Result[(A) => B])(fa: Result[A]): Result[B] = {
        EitherT.apply(for {
          faEither <- fa.value
          ffEither <- ff.value
        } yield {
          faEither.toValidated.ap(ffEither.toValidated).toEither
        })
      }

      override def empty[A]: Result[A] =
        EitherT.left(State.pure(NonEmptyList(CommandNotMatched)))

      override def combineK[A](x: Result[A], y: Result[A]): Result[A] = {
        EitherT.apply(for {
          xS <- x.value
          yS <- y.value
        } yield {
          (xS, yS) match {
            case (r@Right(v), _) => r
            case (_, r@Right(v)) => r
            case (Left(e1), Left(e2)) => Left(e1.combine(e2))
          }
        })
      }

      override def pure[A](x: A): Result[A] = EitherT.pure(x)
    }

  def findCommandArgs(field: CommandField): Result[Seq[String]] = {
    def findArgs(args: Seq[String]):
      (Seq[String], NonEmptyList[ParsingError] Either Seq[String]) = {

        val Pattern = s"^${field.name.show}$$".r
        val index = args.indexWhere(Pattern.pattern.matcher(_).matches)
        if (index != -1) {
          args.take(index) -> Right(args.drop(index + 1))
        } else {
          args -> Left(NonEmptyList[ParsingError](CommandNotFound(field)))
        }
    }

    EitherT.apply(for {
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


