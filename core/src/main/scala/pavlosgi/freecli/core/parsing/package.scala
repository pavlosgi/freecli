package pavlosgi.freecli.core

import cats.data.Validated

package object parsing {
  def getOrReportAndExit[E, A](
    v: Validated[ParsingFailure[E], A],
    help: String)
   (implicit ev: Error[E]):
    A = {

    v match {
      case Validated.Valid(r) => r
      case Validated.Invalid(ParsingFailure(args, ers)) =>
        val errorsDisplay =
          s"""
           |${"Errors:".underline.bold.red}
           |
           |${indent(2, ers.toList.map(ev.message).mkString("\n"))}""".stripMargin

        val parsingDetails = args.args.map {
          case ArgumentWithMarking(name, true) =>
            name.yellow

          case a => a.value
        }.mkString(" ")

        val parsingDetailDisplay =
          s"""${"Parsing details:".underline.bold.red + s" (${"used".yellow}, unused)"}
             |
             |$parsingDetails
           """.stripMargin

        println(
          s"""$errorsDisplay
             |
             |$parsingDetailDisplay
             |$help
             |""".stripMargin)

        sys.exit(1)
    }
  }
}
