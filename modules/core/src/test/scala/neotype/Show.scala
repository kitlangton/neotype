package neotype

object Show:
  given Show[String] with
    def show(a: String): String = a

trait Show[-A]:
  def show(a: A): String

  extension [A](a: A) def show(using s: Show[A]): String = s.show(a)
