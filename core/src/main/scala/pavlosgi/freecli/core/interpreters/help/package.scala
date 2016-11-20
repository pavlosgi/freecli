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
    def lengthExclANSI = {
      s.replaceAll("\u001B\\[[\\d;]*[^\\d;]", "").length
    }
  }

  def indent(indentation: Int, s: String): String = {
    val lines = s.split("\n")
    val i = (0 until indentation).foldLeft("")((a, _) => a + " ")

    lines.map(l => s"$i$l").mkString("\n")
  }

  def contentWithTitle(title: String, s: String) = {
    s"""$title
       |$s""".stripMargin
  }

  def optionalContentWithTitle(title: String, s: Option[String]) = {
    s match {
      case None => None
      case Some(v) => Some(contentWithTitle(title, v))
    }
  }

  def optionalPair(v1: Option[String], v2: Option[String]): String = {
    v1 -> v2 match {
      case (Some(v1_), Some(v2_)) => s"$v1_, $v2_"
      case (Some(v1_), None) => s"$v1_"
      case (None, Some(v2_)) => s"$v2_"
      case (None, None) => s""
    }
  }
}




