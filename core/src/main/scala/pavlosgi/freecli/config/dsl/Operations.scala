package pavlosgi
package freecli
package config
package dsl

import algebra._

import java.io.File

trait Operations {
  def arg[B](field: String,
             description: Option[String] = None,
             abbreviation: Option[Char] = None,
             default: Option[B] = None) =

    Arg(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.map(Description.apply)),

        default)

  def opt[B](field: String,
             description: Option[String] = None,
             abbreviation: Option[Char] = None) =

    Opt[B](Field(FieldName(field),
                 abbreviation.map(FieldAbbreviation.apply),
                 description.map(Description.apply)))

  def sub[A, G[_]: Plugin](name: String)
                          (dsl: ConfigDsl[G, A],
                           description: Option[String] = None,
                           default: Option[A] = None): ConfigDsl[G, A] = {

    new ConfigDsl[G, A] {
      def apply[F[_]: ConfigAlgebra[?[_], G]] =
        implicitly[ConfigAlgebra[F, G]].sub(
          SubField(SubFieldName(name),
                   description.map(Description.apply)),
                   dsl,
                   default)
    }
  }

  def int(field: String,
          abbreviation: Option[Char] = None,
          description: Option[String] = None,
          default: Option[Int] = None): Arg[Int] =

    Arg(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              Some(Description(description.getOrElse("an integer")))),
              default)

  def optInt(field: String,
             abbreviation: Option[Char] = None,
             description: Option[String] = None): Opt[Int] =

    Opt(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              Some(Description(description.getOrElse("an integer")))))

  def string(field: String,
             abbreviation: Option[Char] = None,
             description: Option[String] = None,
             default: Option[String] = None): Arg[String] =

    Arg(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.orElse(Some("a string")).map(Description.apply)),
              default)


  def optString(field: String,
                abbreviation: Option[Char] = None,
                description: Option[String] = None): Opt[String] =

    Opt(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.orElse(Some("a string")).map(Description.apply)))

  def boolean(field: String,
              abbreviation: Option[Char] = None,
              description: Option[String] = None,
              default: Option[Boolean] = None): Arg[Boolean] =

    Arg(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.orElse(Some("a boolean")).map(Description.apply)),
              default)

  def optBoolean(field: String,
                 abbreviation: Option[Char] = None,
                 description: Option[String] = None): Opt[Boolean] =

    Opt(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.orElse(Some("a boolean")).map(Description.apply)))

  def file(field: String,
           abbreviation: Option[Char] = None,
           description: Option[String] = None,
           default: Option[File] = None): Arg[File] =

    Arg(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.orElse(Some("a file")).map(Description.apply)),
              default)

  def optFile(field: String,
              abbreviation: Option[Char] = None,
              description: Option[String] = None): Opt[File] =

    Opt(Field(FieldName(field),
              abbreviation.map(FieldAbbreviation.apply),
              description.orElse(Some("a file")).map(Description.apply)))
}

object Operations extends Operations