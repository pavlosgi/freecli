package pavlosgi.freecli.core

import cats.instances.int._
import cats.syntax.eq._

sealed trait Mark
case object Marked extends Mark
case object Unmarked extends Mark

case class Argument(arg: String, mark: Mark) {
  def marked = Argument(arg, Marked)
}
case class Arguments(args: Seq[Argument]) {
  def marked(idx: Int): Arguments = {
    Arguments(
      args.zipWithIndex.map {
        case (a, i) if i === idx =>
          a.marked

        case (a, _) => a
    })
  }

  def unmarked: Seq[Argument] = {
    args.filter {
      case Argument(_, Unmarked) => true
      case _ => false
    }
  }
}

object Arguments {
  def fromStrings(args: Seq[String]): Arguments = {
    Arguments(args.map(a => Argument(a, Unmarked)))
  }
}