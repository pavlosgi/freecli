package pavlosgi
package freecli
package core

import cats.Show

trait FormatterOps {
  implicit class FormatterOptionOps[A](s: Option[A])(implicit ev: Show[A]) {
    def orEmpty: String = s.fold("")(ev.show)
  }

  implicit class FormatterStringOps(s: String) {
    def parenthesis: String = if (s.nonEmpty) s"($s)" else s
    def yellow: String = s"${Console.YELLOW}$s${Console.RESET}"
    def bold: String = s"${Console.BOLD}$s${Console.RESET}"
    def cyan: String = s"${Console.CYAN}$s${Console.RESET}"
    def red: String = s"${Console.RED}$s${Console.RESET}"
    def underline: String = s"${Console.UNDERLINED}$s${Console.RESET}"
    def indent(indent: Int): String = {
      val sp = (0 until indent * 2).foldLeft[String]("")((a, _) => a + " ")
      s"$sp$s"
    }
  }
}
