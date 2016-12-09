package pavlosgi.freecli.core.formatting

class StringFormattingOps(s: String) {
  def bold: String = s"${Console.BOLD}$s${Console.RESET}"
  def cyan: String = s"${Console.CYAN}$s${Console.RESET}"
  def magenta: String = s"${Console.MAGENTA}$s${Console.RESET}"
  def red: String = s"${Console.RED}$s${Console.RESET}"
  def newline: String = s"$s\n"
  def underline: String = s"${Console.UNDERLINED}$s${Console.RESET}"
  def yellow: String = s"${Console.YELLOW}$s${Console.RESET}"
  def white: String = s"${Console.WHITE}$s${Console.RESET}"
  def lengthExclANSI = {
    s.replaceAll("\u001B\\[[\\d;]*[^\\d;]", "").length
  }
}
