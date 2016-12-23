package pavlosgi.freecli.parser

case class CliArguments(args: Seq[CliArgument]) {
  def usable = args.filter(_.isUsable)
  def unusable = args.filterNot(_.isUsable)
}
