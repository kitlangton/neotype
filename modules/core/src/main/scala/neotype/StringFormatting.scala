package neotype

private[neotype] object StringFormatting:
  extension (string: String)
    def red: String        = Console.RED + string + Console.RESET
    def green: String      = Console.GREEN + string + Console.RESET
    def blue: String       = Console.BLUE + string + Console.RESET
    def cyan: String       = Console.CYAN + string + Console.RESET
    def magenta: String    = Console.MAGENTA + string + Console.RESET
    def yellow: String     = Console.YELLOW + string + Console.RESET
    def bold: String       = Console.BOLD + string + Console.RESET
    def underlined: String = Console.UNDERLINED + string + Console.RESET
    def dim: String        = "\u001B[2m" + string + Console.RESET
