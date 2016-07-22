package pavlosgi
package freecli
package config
package algebra

import cats.{Applicative, Show}

trait Plugin[G[_]]

abstract class ApplyConfigAlgebra[G[_]: Plugin, A] {
  def apply[F[_]: ConfigAlgebra[?[_], G]]: F[A]
}

abstract class ConfigAlgebra[F[_], G[_]: Plugin] extends Applicative[F] {
  def arg[A, B](field: Field,
                f: B => A,
                default: Option[B])
               (implicit ev: G[B]): F[A]

  def opt[A, B](field: Field,
                f: Option[B] => A)
               (implicit ev: G[B]): F[A]

  def sub[A](field: SubField,
             f: ApplyConfigAlgebra[G, A],
             default: Option[A]): F[A]
}

case class Field(name: FieldName, abbreviation: Option[FieldAbbreviation], description: Option[Description])
object Field {
  implicit object showInstance extends Show[Field] {
    override def show(f: Field): String = {
      val abbr = f.abbreviation.map { a =>
        s" | ${FieldAbbreviation.showInstance.show(a)}"
      }.getOrElse("")

      val descr = f.description.map { d =>
                    s" ${Description.showInstance.show(d)}"
                  }.getOrElse("")

      s"${FieldName.showInstance.show(f.name)}$abbr$descr"
    }
  }
}

case class FieldAbbreviation(abbr: Char)
object FieldAbbreviation {
  implicit object showInstance extends Show[FieldAbbreviation] {
    override def show(f: FieldAbbreviation): String = s"-${f.abbr}"
  }
}

class FieldName private(val name: String) {
  override def toString = FieldName.showInstance.show(this)
}

object FieldName {
  def apply(name: String): FieldName = {
    val fname = name.replaceAll(" ", "-").replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
    new FieldName(fname)
  }

  implicit object showInstance extends Show[FieldName] {
    override def show(f: FieldName): String = s"--${f.name}"
  }
}

case class Description(value: String)
object Description {
  implicit object showInstance extends Show[Description] {
    override def show(f: Description): String = s"${f.value}"
  }
}

case class SubField(name: SubFieldName, description: Option[Description])
object SubField {
  implicit object showInstance extends Show[SubField] {
    override def show(f: SubField): String = {
      val descr = f.description.map { d =>
                    s" - ${Description.showInstance.show(d)}"
                  }.getOrElse("")

      s"${SubFieldName.showInstance.show(f.name)}$descr"
    }
  }
}


class SubFieldName private(val name: String) {
  override def toString = SubFieldName.showInstance.show(this)
}

object SubFieldName {
  def apply(name: String): SubFieldName = {
    val fname = name.replaceAll(" ", "-").replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
    new SubFieldName(fname)
  }

  implicit object showInstance extends Show[SubFieldName] {
    override def show(f: SubFieldName): String = s"${f.name}"
  }
}