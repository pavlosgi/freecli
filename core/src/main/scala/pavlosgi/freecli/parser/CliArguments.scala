package pavlosgi.freecli.parser

case class CliArguments(args: Seq[CliArgument]) {
  def usable = args.filter(_.isUsable)
  def unusable = args.filterNot(_.isUsable)

  def unusedOutOfOrder: Seq[CliArgument] = {
    val l = args.map(a => (a, !a.isUsable)).dropWhile(_._2)
    l.take(l.indexWhere(_._2)).map(_._1)
  }
}