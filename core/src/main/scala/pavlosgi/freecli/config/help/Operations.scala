package pavlosgi
package freecli
package config
package help

import Instances._

import algebra._
import dsl.ConfigDsl
import pavlosgi.freecli.formatting._

import cats.{Show, ~>}
import cats.data.State
import cats.syntax.show._

trait Operations {
  private type ConfigPrinter[_] = State[ConfigHelp, Unit]

  def usage[G[_]: Plugin, A]
           (p: ConfigDsl[G, A])
           (implicit nat: G ~> Show): String = {

    s"${"Usage".underline}\n\n${genHelp(p).asString(1)}"
  }

  def genHelp[G[_]: Plugin, A]
             (p: ConfigDsl[G, A])
             (implicit nat: G ~> Show): ConfigHelp = {

    p.apply(helpAlgebra).runS(ConfigHelp()).value
  }

  private def helpAlgebra[G[_]: Plugin]
    (implicit nat: G ~> Show): ConfigAlgebra[ConfigPrinter, G] = {

    new ConfigAlgebra[ConfigPrinter, G] {
      override def pure[A](x: A): ConfigPrinter[A] = State.pure(())

      override def ap[A, B](ff: ConfigPrinter[(A) => B])
                           (fa: ConfigPrinter[A]): ConfigPrinter[B] = {

        for {
          h1 <- ff.get
          h2 <- fa.get
        } yield State.pure(ConfigHelp(
          h1.args ++ h2.args,
          h1.opts ++ h2.opts,
          h1.subConfigs ++ h2.subConfigs))
      }

      def arg[A, B](field: Field,
                    value: B => A,
                    default: Option[B])
                   (implicit ev: G[B]): ConfigPrinter[A] = {

        implicit def show = nat.apply(ev)
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.args :+
                  s"${field.show} ${defaultStr(default)}"))

        } yield ()
      }

      def opt[A, B](field: Field,
                    value: Option[B] => A)
                   (implicit ev: G[B]): ConfigPrinter[A] = {

        implicit def show = nat.apply(ev)
        for {
          _ <- State.modify[ConfigHelp](h => h.copy(args = h.args :+
            s"${field.show}"))

        } yield ()
      }

      def sub[A](field: SubField,
                 value: ConfigDsl[G, A],
                 default: Option[A]): ConfigPrinter[A] = {

        for {
          _ <- State.modify[ConfigHelp] { h =>
                h.copy(subConfigs =
                         h.subConfigs :+
                           s"${field.show}${defaultStr(default.map(_.toString))}"
                             -> genHelp(value))}

        } yield ()
      }

    }
  }

  def defaultStr[A](default: Option[A])
                   (implicit ev: Show[A]): String = {

    default match {
      case None => ""
      case Some(d) => s" (default = ${ev.show(d)})"
    }
  }
}

object Operations extends Operations