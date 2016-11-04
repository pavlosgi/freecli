//package pavlosgi.freecli.core.command.interpreters
//
//import cats.data._
//import cats.syntax.all._
//
//import pavlosgi.freecli.core.api.HelpState
//import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}
//import pavlosgi.freecli.core.command.api.{Algebra, Command, CommandField}
//
//package object help {
//  type Result[_] = State[HelpState, Unit]
//
//  def showCommandHelp[A, G[_]](
//     dsl: G[A])
//    (implicit ev: G[A] => Result[Command]):
//     String = {
//
//    "Usage".bold.underline.newlineLeft.newline.newline +
//      ev(dsl).runS(HelpState(2, "")).value.text.newline
//  }
//
//  implicit def helpCommandAlgebra(implicit ev: ConfigAlgebra[Result]) = new Algebra[Result] {
//
//    def genParentCommandFieldHelp(field: CommandField): Result[Unit] = {
//      val s = String.format("%-15s   %s", field.name.show.cyan, field.description.fold("")(_.show.cyan))
//      for {
//        curr <- State.get[HelpState]
//        space = (0 until curr.indentation)
//                  .foldLeft[String]("")((a, _) => a + " ")
//
//        _ <- State.set(HelpState(curr.indentation, text = space + s.newline))
//      } yield ()
//    }
//
//    def genCommandFieldHelp(field: CommandField): Result[Unit] = {
//      val s = String.format("%-15s   %s", field.name.show, field.description.fold("")(_.show))
//      for {
//        curr <- State.get[HelpState]
//        space = (0 until curr.indentation)
//                  .foldLeft[String]("")((a, _) => a + " ")
//
//        _ <- State.set(HelpState(curr.indentation, text = space + s.newline))
//      } yield ()
//    }
//
//    override def cmd(
//      field: CommandField,
//      run: => Unit):
//      Result[Command] = genCommandFieldHelp(field)
//
//    override def cmdWithSubcommands[G[_]](
//      field: CommandField,
//      subcommands: G[Command])
//     (implicit ev: G[Command] => Result[Command]):
//      Result[Command] = {
//
//      for {
//        curr <- genParentCommandFieldHelp(field).get
//        _    <- State.set(curr.copy(indentation = curr.indentation + 2))
//        sub  <- ev(subcommands).get
//        _    <- State.set(HelpState(curr.indentation, text = curr.text + "\n" + sub.text))
//      } yield ()
//    }
//
//    override def cmdWithConfig[H[_], A](
//      field: CommandField,
//      config: H[A],
//      run: A => Unit)
//     (implicit ev: H[A] => Result[A]):
//      Result[Command] = {
//
//      for {
//        curr <- genCommandFieldHelp(field).get
//        _    <- State.set(curr.copy(indentation = curr.indentation + 2))
//        conf <- ev(config).get
//        _    <- State.set(conf.copy(indentation = conf.indentation - 2))
//      } yield ()
//    }
//
//    override def cmdWithConfigAndSubcommands[G[_], H[_], A](
//      field: CommandField,
//      config: H[A],
//      subcommands: G[Command])
//     (implicit ev: G[Command] => Result[Command],
//      ev2: H[A] => Result[A]):
//      Result[Command] = {
//
//      for {
//        curr <- genParentCommandFieldHelp(field).get
//        _    <- State.set(curr.copy(indentation = curr.indentation + 2))
//        sub  <- ev(subcommands).get
//        _    <- State.set(HelpState(curr.indentation, text = curr.text + "\n" + sub.text))
//      } yield ()
//    }
//
//    override def combineK[A](x: Result[A], y: Result[A]): Result[A] = {
//      for {
//        xS <- x.get
//        yS <- y.get
//        _  <- State.set(xS.copy(text = xS.text + "\n" + yS.text))
//      } yield ()
//    }
//
//    override def pure[A](x: A): Result[A] = State.pure(())
//
//    override def empty[A]: Result[A] = State.pure(())
//
//    override def ap[A, B](ff: Result[(A) => B])(fa: Result[A]): Result[B] = {
//      for {
//        ff1 <- ff.get
//        fa1 <- fa.get
//        _   <- State.set(ff1.copy(text = ff1.text + "\n" + fa1.text))
//      } yield ()
//    }
//  }
//
//  implicit class StringOps(s: String) {
//    def underline: String = s"${Console.UNDERLINED}$s${Console.RESET}"
//    def newline: String = s + "\n"
//    def newlineLeft: String = "\n" + s
//    def yellow: String = s"${Console.YELLOW}$s${Console.RESET}"
//    def cyan: String = s"${Console.CYAN}$s${Console.RESET}"
//    def bold: String = s"${Console.BOLD}$s${Console.RESET}"
//  }
//}