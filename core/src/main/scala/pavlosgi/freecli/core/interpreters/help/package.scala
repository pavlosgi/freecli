package pavlosgi.freecli.core.interpreters

import cats.data._

package object help {
  type Result[A] = State[HelpState, Unit]

  object Result {
    def indentation(f: Int => Int): Result[Unit] = {
      for {
        hs <- State.get[HelpState]
        _ <- State.set(hs.copy(indentation = f(hs.indentation)))
      } yield ()
    }

    def newline: Result[Unit] = {
      for {
        hs <- State.get[HelpState]
        _ <- State.set(hs.copy(text = s"${hs.text}\n"))
      } yield ()
    }

    def genHelp(text: String): Result[Unit] = {
      for {
        helpState <- State.get[HelpState]
        space = (0 until helpState.indentation)
          .foldLeft[String]("")((a, _) => a + " ")

        _ <- State.set(HelpState(
              helpState.indentation,
              text = helpState.text + space + text.newline))

      } yield ()
    }
  }

  implicit class StringOps(s: String) {
    def underline: String = s"${Console.UNDERLINED}$s${Console.RESET}"
    def newline: String = s + "\n"
    def newlineLeft: String = "\n" + s
    def yellow: String = s"${Console.YELLOW}$s${Console.RESET}"
    def cyan: String = s"${Console.CYAN}$s${Console.RESET}"
    def bold: String = s"${Console.BOLD}$s${Console.RESET}"
  }
}
