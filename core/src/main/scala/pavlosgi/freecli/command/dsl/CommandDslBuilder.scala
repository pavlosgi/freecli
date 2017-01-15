package pavlosgi.freecli.command.dsl

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.CanProduce

case class CommandDslBuilder[H <: HList, Conf, Run](list: H) {
  def apply[H2 <: HList, Conf2, Run2, HH2 <: HList, Out](
    f: CommandDslBuilder[H2, Conf2, Run2])
   (implicit ev: Prepend.Aux[H, H2, HH2],
    ev2: Conf =:= HNil,
    ev3: Run =:= HNil,
    ev4: CanProduce.Aux[CommandDslBuilder[HH2, Conf2, Run2], Out]):
    Out = {

    ev4(CommandDslBuilder[HH2, Conf2, Run2](list ++ f.list))
  }
}