package pavlosgi.freecli.option.parser

import pavlosgi.freecli.option.api.Action
import pavlosgi.freecli.option.dsl.OptionDsl
import pavlosgi.freecli.parser.CliParser

object ops extends ParserOps
trait ParserOps {
  private[freecli] def parseOptionNonStrict[T](
    dsl: OptionDsl[T]):
    CliParser[Action, OptionParsingError, T] = {

    for {
      r <- dsl.foldMap(OptionParserInterpreter)
      args <- CliParser.getArgs
      _ <- args.unusedOutOfOrder match {
        case Nil => CliParser.success[Action, OptionParsingError, Unit](())
        case l   => CliParser.error[Action, OptionParsingError, Unit](
          ArgumentsDidNotMatchAnyOptions(l.map(_.name)))
      }
    } yield r
  }

  def parseOption[T](
    dsl: OptionDsl[T]):
    CliParser[Action, OptionParsingError, T] = {

    parseOptionNonStrict(dsl).failIfNotAllArgumentsUsed(
      args => AdditionalArgumentsFound(args.args.map(_.name)))
  }
}
