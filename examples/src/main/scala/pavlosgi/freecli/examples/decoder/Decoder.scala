package pavlosgi.freecli.examples.decoder

import cats.data.{Validated, ValidatedNel}

import pavlosgi.freecli.argument.all._
import pavlosgi.freecli.core.api.{StringDecoder, StringDecoderError}

object Decoder extends App {
  sealed trait Fruit
  case object Apple extends Fruit
  case object Pear extends Fruit
  case object Orange extends Fruit

  implicit object fruitStringDecoder extends StringDecoder[Fruit] {
    override def apply(value: String): ValidatedNel[StringDecoderError, Fruit] = {
      value match {
        case v if v.equalsIgnoreCase("Apple") => Validated.valid(Apple)
        case v if v.equalsIgnoreCase("Pear") => Validated.valid(Pear)
        case v if v.equalsIgnoreCase("Orange") => Validated.valid(Orange)
        case v =>
          Validated.invalidNel(StringDecoderError(
            s"$v did not match any of (Apple, Pear, Orange)"))
      }
    }

    override def toString(v: Fruit): String = {
      v match {
        case Apple => "Apple"
        case Pear => "Pear"
        case Orange => "Orange"
      }
    }
  }

  val res = runArgumentOrFail(arg[Fruit])(args)
  println(res)
}
