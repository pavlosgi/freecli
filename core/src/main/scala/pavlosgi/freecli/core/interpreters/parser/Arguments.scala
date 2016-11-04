package pavlosgi.freecli.core.interpreters.parser

import pavlosgi.freecli.core.api.config.FieldAbbreviation

class Arguments private(val args: Seq[String])

object Arguments {
  def unapply(arguments: Arguments): Option[Seq[String]] = Some(arguments.args)

  def apply(args: Seq[String]): Arguments = {
    new Arguments(
      args.foldLeft(Seq.empty[String]) {
      case (curr, arg) =>
        curr ++ FieldAbbreviation.splitMultiFieldAbbreviation(arg)
    })
  }
}
