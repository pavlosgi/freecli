package pavlosgi.freecli.core.dsl.command

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.Selector

import pavlosgi.freecli.core.api.AlgebraDependency
import pavlosgi.freecli.core.api.command.{Command, PartialCommand}
import pavlosgi.freecli.core.api.command.Algebra
import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}

case class CommandPartsBuilder[H <: HList, Conf, Run](list: H) {
  def ::[L <: HList, Conf2, Run2](
    c: CommandPartsBuilder[L, Conf2, Run2])
    (implicit ev: PartsMerger[L, Conf2, Run2, H, Conf, Run]):
     ev.Out = {

    ev(c, this)
  }

  def apply[L <: HList, P <: HList, Conf2, Run2](
    f: CommandPartsBuilder[L, Conf2, Run2])
   (implicit ev: CanProduceCommandField[H],
    ev2: CanProduceDsl[L, Conf2, Run2]):
    ev2.Out = {

    ev2(ev(list), f)
  }
}

object CommandPartsBuilder {
  implicit def toCommandDsl[H <: HList, Run](
    p: CommandPartsBuilder[H, _, Run])
   (implicit ev: Selector[H, CommandDsl[PartialCommand[Run]]],
    ev2: UnitOrEmpty[Run]):
    CommandDsl[Command] = {

    ev(p.list).map(_.f(ev2.empty))
  }

  implicit def cpb2CommandDsl[F[_], C[_], H <: HList, Conf, Run](
    implicit ev: Algebra[F],
    ev2: AlgebraDependency[ConfigAlgebra, F, C],
    ev3: Selector[H, CommandDsl[PartialCommand[Run]]],
    ev4: UnitOrEmpty[Run]):
    CommandPartsBuilder[H, Conf, Run] => F[Command] = {

    cpb => toCommandDsl(cpb).apply[F, C]
  }

  sealed trait UnitOrEmpty[H] {
    def empty: H
  }

  object UnitOrEmpty {
    implicit def hnilEmpty = new UnitOrEmpty[HNil] {
      override def empty: HNil = HNil
    }

    implicit def unitEmpty = new UnitOrEmpty[Unit] {
      override def empty: Unit = ()
    }
  }
}
