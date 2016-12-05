package pavlosgi.freecli.config.interpreters

import cats.data._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.arguments.interpreters.{parser => A}
import pavlosgi.freecli.core.{Arguments, ResultTS}
import pavlosgi.freecli.core.ResultTS
import pavlosgi.freecli.options.interpreters.{parser => O}

package object parser {
  type ResultT[A] = ResultTS[ConfigParsingError, Arguments, A]

  def parseConfig[G, A](
    args: Seq[String])
   (dsl: ConfigDsl[A]):
    ValidatedNel[ConfigParsingError, A] = {

    ResultTS.run(Arguments(args))(dsl.foldMap(configAlgebraParser)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(argsLeft), ers))
      }
  }

  implicit object configAlgebraParser extends (Algebra ~> ResultT) {
    def apply[A](fa: Algebra[A]): ResultT[A] = {
      fa match {
        case Args(args) =>
          args.foldMap(A.argumentsAlgebraParser)
            .leftMap(ers => NonEmptyList.of(ArgumentErrors(ers)))

        case Opts(opts) =>
          opts.foldMap(O.optionAlgebraParser)
            .leftMap(ers => NonEmptyList.of(OptionErrors(ers)))

        case OptsAndArgs(opts, args, f) =>
          for {
            optsRes <- apply(Opts(opts))
            argsRes <- apply(Args(args))
          } yield f(optsRes, argsRes)
      }
    }
  }
}