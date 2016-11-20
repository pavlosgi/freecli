package pavlosgi.freecli.core.interpreters.help

import cats.data._

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

package object config {
  type Result[A] = State[HelpState, Unit]

  def configHelp[G, A](
    dsl: G)
   (implicit ev: G => Result[A]):
    String = {

    val result = ev(dsl).runS(HelpState.empty).value
    val argsOneLine = result.arguments.map(ArgumentsHelp.oneline)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options] $argsOneLine
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  implicit object configAlgebraHelp extends Algebra[Result] {
    override def arg[T, A](
      details: ArgumentDetails,
      f: T => A,
      g: StringDecoder[T]):
      Result[A] = {

      for {
        hs <- State.get[HelpState]
        _  <- State.set(hs.addArgument(details))
      } yield ()
    }

    override def requiredOpt[T, A](
      field: Field,
      f: T => A,
      g: StringDecoder[T],
      default: Option[T]):
      Result[A] = {

      for {
        hs <- State.get[HelpState]
        _  <- State.set(hs.addOption(
          OptionFieldHelp(field, default.map(g.toString))))

      } yield ()
    }

    override def opt[T, A](
      field: Field,
      f: Option[T] => A,
      g: StringDecoder[T]):
      Result[A] = {

      for {
        hs <- State.get[HelpState]
        _  <- State.set(hs.addOption(OptionFieldHelp(field)))
      } yield ()
    }

    override def flag[A](
      field: Field,
      f: Boolean => A):
      Result[A] = {

      for {
        hs <- State.get[HelpState]
        _  <- State.set(hs.addOption(OptionFieldHelp(field)))
      } yield ()
    }

    override def sub[G, A](
      description: Description,
      dsl: G)
     (implicit ev: G => Result[A]):
      Result[A] = {

      for {
        hs  <- State.get[HelpState]
        shs = ev(dsl).get.runS(HelpState.empty).value
        _   <- State.set(hs.addOption(SubHelp(description, shs)))
      } yield ()
    }

    override def pure[A](x: A): Result[A] = State.pure(())

    override def ap[A, B]
      (ff: Result[A => B])
      (fa: Result[A]):
       Result[B] = {

      for {
        ff1 <- ff.get
        fa1 <- fa.get
      } yield ()
    }
  }
}