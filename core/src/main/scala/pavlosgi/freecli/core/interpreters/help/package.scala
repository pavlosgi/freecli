package pavlosgi.freecli.core.interpreters

package object help {
  implicit class StringOps(s: String) {
    def underline: String = s"${Console.UNDERLINED}$s${Console.RESET}"
    def newline: String = s + "\n"
    def newlineLeft: String = "\n" + s
    def yellow: String = s"${Console.YELLOW}$s${Console.RESET}"
    def cyan: String = s"${Console.CYAN}$s${Console.RESET}"
    def bold: String = s"${Console.BOLD}$s${Console.RESET}"
  }
}
