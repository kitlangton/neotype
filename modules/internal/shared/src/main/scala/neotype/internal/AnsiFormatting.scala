package neotype.internal

// ANSI formatting helpers for error messages.
object AnsiFormatting:
  // NO_COLOR standard: https://no-color.org/
  private val noColor: Boolean       = Option(System.getenv("NO_COLOR")).exists(_.nonEmpty)
  private val hasTTY: Boolean        = System.console() != null
  private val colorsEnabled: Boolean = !noColor && hasTTY

  private def styled(code: String, text: String): String =
    if colorsEnabled then code + text + Console.RESET else text

  extension (string: String)
    def red: String        = styled(Console.RED, string)
    def green: String      = styled(Console.GREEN, string)
    def blue: String       = styled(Console.BLUE, string)
    def cyan: String       = styled(Console.CYAN, string)
    def magenta: String    = styled(Console.MAGENTA, string)
    def yellow: String     = styled(Console.YELLOW, string)
    def white: String      = styled(Console.WHITE, string)
    def reset: String      = styled(Console.RESET, string)
    def bold: String       = styled(Console.BOLD, string)
    def underlined: String = styled(Console.UNDERLINED, string)
    def dim: String        = styled("\u001B[2m", string)
