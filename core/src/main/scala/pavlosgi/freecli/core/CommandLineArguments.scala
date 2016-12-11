package pavlosgi.freecli.core

import cats.syntax.all._
import cats.instances.all._

case class ArgumentWithMarking(value: String, isMarked: Boolean)

case class ExtractSingle(args: CommandLineArguments, res: Option[String])
case class ExtractPair(
  args: CommandLineArguments,
  name: Option[String],
  value: Option[String])

case class CommandLineArguments(args: Seq[ArgumentWithMarking]) {
  def extractNext: ExtractSingle = {
    args.zipWithIndex.collectFirst {
      case (ArgumentWithMarking(value, false), idx) =>
        ExtractSingle(this.mark(idx), Some(value))

    }.getOrElse(ExtractSingle(this, None))
  }

  def extractNextIfMatches(f: String => Boolean): ExtractSingle = {
    args.zipWithIndex.find(a => !a._1.isMarked) match {
      case Some((a, idx)) if f(a.value) =>
        ExtractSingle(this.mark(idx), Some(a.value))

      case _ =>
        ExtractSingle(this, None)
    }
  }

  def extract(f: String => Boolean): ExtractSingle = {
    args.zipWithIndex.collectFirst {
      case (ArgumentWithMarking(value, false), idx) if f(value) =>
        ExtractSingle(this.mark(idx), Some(value))

    }.getOrElse(ExtractSingle(this, None))
  }

  def extractPair(f: String => Boolean): ExtractPair = {
    args.zipWithIndex.sliding(2).collectFirst {
      case Seq((arg1, idx1), (arg2, idx2)) if !arg1.isMarked && !arg2.isMarked && f(arg1.value) =>
        ExtractPair(
          this.mark(idx1).mark(idx2), Some(arg1.value), Some(arg2.value))

      case Seq((arg1, _)) if f(arg1.value) =>
        ExtractPair(this, Some(arg1.value), None)

    }.getOrElse(ExtractPair(this, None, None))
  }

  private def mark(idx: Int): CommandLineArguments = {
    val newArgs =
      args.zipWithIndex.map {
        case (ArgumentWithMarking(v, _), index) if index === idx =>
          ArgumentWithMarking(v, isMarked = true)

        case (a, _) => a
      }

    new CommandLineArguments(newArgs)
  }

  def markAllBeforeLastMarked: CommandLineArguments = {
    val lastMarked = args.filter(_.isMarked).zipWithIndex.map(_._2).lastOption.getOrElse(0)
    new CommandLineArguments(
      args.zipWithIndex.map {
        case (ArgumentWithMarking(v, _), idx) if idx < lastMarked =>
          ArgumentWithMarking(v, isMarked = true)

        case (a, _) => a
    })
  }

  def unmarked: Seq[String] = {
    args.filterNot(_.isMarked).map(_.value)
  }
}

object CommandLineArguments {
  def fromArgs(args: Seq[String]): CommandLineArguments = {
    new CommandLineArguments(
      args.map(a => ArgumentWithMarking.apply(a, isMarked = false)))
  }
}
