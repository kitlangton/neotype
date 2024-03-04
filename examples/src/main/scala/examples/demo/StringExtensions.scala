package examples.demo

extension (self: String)
  def blue: String   = scala.Console.BLUE + self + scala.Console.RESET
  def cyan: String   = scala.Console.CYAN + self + scala.Console.RESET
  def red: String    = scala.Console.RED + self + scala.Console.RESET
  def yellow: String = scala.Console.YELLOW + self + scala.Console.RESET
