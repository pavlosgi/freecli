package pavlosgi.freecli.core.interpreters.help

import cats.data._

import pavlosgi.freecli.core.api.config._

package object config {
  type Result[A] = State[ConfigHelpState, Unit]

  def configHelp[G, A](
    dsl: G)
   (implicit ev: G => Result[A]):
    String = {

    ev(dsl).runS(
      ConfigHelpState(OptionHelpState.empty, ArgumentHelpState.empty))
      .value.display
  }

  implicit object configAlgebraHelp extends Algebra[Result] {
    override def arg[T, A](
      details: ArgumentDetails,
      f: T => A,
      g: StringDecoder[T]):
      Result[A] = addArgument(details)

    override def requiredOpt[T, A](
      field: Field,
      f: T => A,
      g: StringDecoder[T],
      default: Option[T]):
      Result[A] = addField(field)

    override def opt[T, A](
      field: Field,
      f: Option[T] => A,
      g: StringDecoder[T]):
      Result[A] = addField(field)

    override def flag[A](
      field: Field,
      f: Boolean => A):
      Result[A] = addField(field)

    override def sub[G, A](
      description: Description,
      dsl: G)
     (implicit ev: G => Result[A]):
      Result[A] = addSub(description, ev(dsl).runS(ConfigHelpState.empty).value)

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

    def addArgument(a: ArgumentDetails): Result[Unit] = {
      for {
        h <- State.get[ConfigHelpState]
        _ <- State.set(h.addArgument(a))
      } yield ()
    }

    def addField(f: Field): Result[Unit] = {
      for {
        h <- State.get[ConfigHelpState]
        _ <- State.set(h.addOption(FieldHelp(f)))
      } yield ()
    }

    def addSub(d: Description, c: ConfigHelpState): Result[Unit] = {
      for {
        h <- State.get[ConfigHelpState]
        _ <- State.set(h.addOption(SubHelp(d, c)))
      } yield ()
    }
  }
}