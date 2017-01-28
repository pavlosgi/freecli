package pavlosgi.freecli.examples

object Argument {
  import pavlosgi.freecli.core.all._
  import pavlosgi.freecli.argument.all._

  val dsl = string
  val res: String = runArgumentOrFail(dsl)(Seq("one"))

  val dsl2 = string -~ name("arg1")
  val res2: String = runArgumentOrFail(dsl2)(Seq("one"))

  val dsl3 = string -~ name("arg1") -~ des("argument description")
  val res3: String =
    runArgumentOrFail(dsl3)(Seq("one"))

  case class Arguments(arg1: String, arg2: Int)
  val dsl4 =
    group[Arguments] {
      string ::
      int
    }

  val res4: Arguments =
    runArgumentOrFail(dsl4)(Seq("one", "2"))

  val dsl5 =
    groupT {
      string ::
      int
    }

  val res5: (String, Int) =
    runArgumentOrFail(dsl5)(Seq("one", "2"))
}

object Option {
  import pavlosgi.freecli.core.all._
  import pavlosgi.freecli.option.all._

  val dsl = string --"opt1"
  val res: Option[String] =
    runOptionOrFail(dsl)(Seq("--opt1", "one"))

  val dsl2 = string --"opt1" -~ des("option description")
  val res2: Option[String] =
    runOptionOrFail(dsl2)(Seq("--opt1", "one"))

  val dsl3 = string -'o'
  val res3: Option[String] =
    runOptionOrFail(dsl3)(Seq("-o", "one"))

  val dsl4 = string -'o' -~ des("option description")
  val res4: Option[String] =
    runOptionOrFail(dsl4)(Seq("-o", "one"))

  val dsl5 = string --"opt1" -'o'
  val res5: Option[String] =
    runOptionOrFail(dsl5)(Seq("--opt1", "one"))

  val dsl6 = string --"opt1" -'o' -~ des("option description")
  val res6: Option[String] =
   runOptionOrFail(dsl6)(Seq("-o", "one"))

  case class Options(
    opt1: Option[String],
    opt2: Int,
    opt3: Int,
    opt4: Boolean)

  val dsl7 =
    group[Options] {
      string  --"opt1"          ::
      int     --"opt2" -~ req   ::
      int     --"opt3" -~ or(1) ::
      flag    --"opt4" ::
      help    --"help" ::
      version --"version" -~ value("v1.0")
    }

  val res7: Options =
    runOptionOrFail(dsl7)(
      Seq(
        "--opt1", "one",
        "--opt2", "two",
        "--opt3", "three",
        "--opt4"
      ))

  val res7b: Options =
    runOptionOrFail(dsl7)(
      Seq(
        "--opt2", "two",
        "--opt3", "three"))
}

object Config {
  import pavlosgi.freecli.core.all._
  import pavlosgi.freecli.config.all._

  val dsl = O.string --"opt1"
  val res: Option[String] =
    runConfigOrFail(dsl)(Seq("--opt1", "one"))

  val dsl2 = O.string --"opt1" -~ des("option description")
  val res2: Option[String] =
    runConfigOrFail(dsl2)(Seq("--opt1", "one"))

  val dsl3 = O.string -'o'
  val res3: Option[String] =
    runConfigOrFail(dsl3)(Seq("-o", "one"))

  val dsl4 = O.string -'o' -~ des("option description")
  val res4: Option[String] =
    runConfigOrFail(dsl4)(Seq("-o", "one"))

  val dsl5 = O.string --"opt1" -'o' -~ or ("1")
  val res5: String =
    runConfigOrFail(dsl5)(Seq("--opt1", "one"))

  val dsl6 = O.string --"opt1" -'o' -~ req -~ des("option description")
  val res6: String =
    runConfigOrFail(dsl6)(Seq("--opt1", "one"))

  case class Config(
    opt1: Option[String],
    opt2: Int,
    opt3: Int,
    opt4: Boolean,
    arg1: String,
    arg2: Int)

  val dsl7 =
    group[Config] {
      O.string --"opt1"          ::
      O.int    --"opt2" -~ req   ::
      O.int    --"opt3" -~ or(1) ::
      flag     --"opt4"          ::
      string                     ::
      int
    }

  val res7: Config =
    runConfigOrFail(dsl7)(
      Seq(
       "--opt1", "one",
       "--opt2", "two",
       "--opt3", "three",
       "--opt4",
       "five",
       "six"))
}

object Command {
  import pavlosgi.freecli.core.all._
  import pavlosgi.freecli.config.all._
  import pavlosgi.freecli.command.all._

  case class Command1Config(opt1: Option[Int], opt2: String)
  case class Command2Config(opt3: Int, arg1: String)
  case class Command3Config(arg2: String)

  val dsl =
    cmd("command1") {
      takesG[Command1Config] {
        O.help   --"help" ::
        O.int    --"opt1" -~ des("opt1 description") ::
        O.string --"opt2" -~ req
      } ::
      cmd("command2") {
        takesG[Command2Config] {
          O.help --"help" ::
          O.int  --"opt3" -~ or(1) ::
          string -~ name("arg1")
        } ::
        runs[ParentWith[Command1Config, Command2Config]] { conf =>
          println(conf)
        }
      } ::
      cmd("command3") {
        takesG[Command3Config] {
          O.help --"help" ::
          string -~ name("arg2")
        } ::
        runs[ParentWith[Command1Config, Command3Config]] { conf =>
          println(conf)
        }
      }
    }

  val res: Unit =
    runCommandOrFail(dsl)(
      Seq(
       "command1", "--opt1", "1", "opt2", "two",
         "command2", "--opt3", "3", "four")).run

  /*
  //Fails and prints errors and help for failing command
  runCommandOrFail(dsl)(
    Seq(
     "command1", "--opt1", "1", "opt2", "two",
       "command2", "--opt3", "3")).run
  */

}

object CustomDecoder {
  import cats.data.{Validated, ValidatedNel}

  import pavlosgi.freecli.argument.all._
  import pavlosgi.freecli.core.api.{StringDecoder, StringDecoderError}

  sealed trait FooBar
  case object Foo extends FooBar
  case object Bar extends FooBar

  implicit object fooBarStringDecoder extends StringDecoder[FooBar] {
    override def apply(value: String): ValidatedNel[StringDecoderError, FooBar] = {
      value match {
        case v if v.equalsIgnoreCase("Foo") => Validated.valid(Foo)
        case v if v.equalsIgnoreCase("Bar") => Validated.valid(Bar)
        case v =>
          Validated.invalidNel(StringDecoderError(s"$v did not match any of (Foo, Bar)"))
      }
    }

    override def toString(v: FooBar): String = {
      v match {
        case Foo => "Foo"
        case Bar => "Bar"
      }
    }
  }

  val x: FooBar = runArgumentOrFail(arg[FooBar])(Seq("Foo"))
}

