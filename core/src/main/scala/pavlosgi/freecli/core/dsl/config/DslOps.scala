package pavlosgi.freecli.core.dsl.config

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend}

import pavlosgi.freecli.core.api.config._

trait FieldNameOps[H <: HList] { self: Builder[H] =>
  def --(
    name: String)
   (implicit ev: Prepend[H, FieldName :: HNil],
    ev2: NotContainsConstraint[H, FieldName]) =

    append(FieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, FieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, FieldAbbreviation]) =

    append(FieldAbbreviation(abbr))
}

trait FieldDescriptionOps[H <: HList] { self: Builder[H] =>
  def -?(
    description: String)
   (implicit ev: Prepend[H, Option[Description] :: HNil],
    ev2: NotContainsConstraint[H, Option[Description]]) =

    append(Option(Description(description)))
}

trait DefaultOps[H <: HList, T] { self: Builder[H] =>
  def -|(
    default: T)
   (implicit ev: Prepend[H, DefaultValue[T] :: HNil],
    ev2: NotContainsConstraint[H, DefaultValue[_]]) =

    append(DefaultValue(default))
}



trait ApplyOps[H <: HList, T] { self: Builder[H] =>
  def apply[Conf](
    f: ConfigDsl[Conf])
  (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T],
    ev2: NotContainsConstraint[H, ConfigDsl[_]],
    ev3: Prepend[H, ConfigDsl[T] :: HNil]) = {

    append(f.map(c => folder(c :: HNil, Option.empty[T])))
  }
}