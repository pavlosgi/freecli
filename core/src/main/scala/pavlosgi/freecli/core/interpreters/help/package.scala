package pavlosgi.freecli.core.interpreters

package object help {
  implicit class StringOps(s: String) {
    def bold: String = s"${Console.BOLD}$s${Console.RESET}"
    def cyan: String = s"${Console.CYAN}$s${Console.RESET}"
    def magenta: String = s"${Console.MAGENTA}$s${Console.RESET}"
    def newline: String = s"$s\n"
    def underline: String = s"${Console.UNDERLINED}$s${Console.RESET}"
    def yellow: String = s"${Console.YELLOW}$s${Console.RESET}"
    def white: String = s"${Console.WHITE}$s${Console.RESET}"
  }

  def indent(indentation: Int, s: String): String = {
    val lines = s.split("\n")
    val i = (0 until indentation).foldLeft("")((a, _) => a + " ")

    lines.map(l => s"$i$l").mkString("\n")
  }

  def optionalFragment(o: Option[String]): String = {
    o.fold("")(v => s"\n$v")
  }

}
