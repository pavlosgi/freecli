package pavlosgi.freecli.core.interpreters.parser

import cats.data._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.ConfigDsl
import pavlosgi.freecli.core.interpreters.ResultTS
import pavlosgi.freecli.core.interpreters.parser.{arguments => A}
import pavlosgi.freecli.core.interpreters.parser.{options => O}

package object config {
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