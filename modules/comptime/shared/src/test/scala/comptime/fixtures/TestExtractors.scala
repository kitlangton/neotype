package comptime

object TestExtractors:
  object Even:
    def unapply(n: Int): Boolean = n % 2 == 0

  object Half:
    def unapply(n: Int): Option[Int] = if n % 2 == 0 then Some(n / 2) else None

  object Split:
    def unapply(s: String): Option[(String, String)] =
      s.split(":", 2) match
        case Array(a, b) => Some((a, b))
        case _           => None

  object Parts:
    def unapplySeq(s: String): Option[(String, Seq[String])] =
      val parts = s.split("-", -1).toList
      parts.headOption.map(head => (head, parts.drop(1)))

  object Always:
    def unapply(n: Int): Option[Unit] =
      if n > 0 then Some(()) else None

  object Overloaded:
    def unapply(n: Int): Option[Int] =
      if n > 0 then Some(n) else None
    def unapply(s: String): Option[Int] =
      Some(s.length)

  object IsEven:
    def unapply(n: Int): Boolean =
      n % 2 == 0

  object PersonBox:
    final case class Person(name: String, age: Int)
    def unapply(n: Int): Option[Person] =
      if n >= 0 then Some(Person("p", n)) else None

  object Two:
    def unapplySeq(n: Int): Option[Seq[Int]] =
      if n >= 0 then Some(List(n, n + 1)) else None

  object ArrParts:
    def unapplySeq(n: Int): Option[(Int, Seq[Int])] =
      if n >= 0 then Some((n, List(n + 1, n + 2))) else None

  object TrioRest:
    def unapplySeq(n: Int): Option[(Int, Int, Seq[Int])] =
      if n >= 0 then Some((n, n + 1, List(n + 2, n + 3))) else None

  object Triple:
    def unapply(n: Int): Option[(Int, Int, Int)] =
      if n >= 0 then Some((n, n + 1, n + 2)) else None

  object PairBox:
    final case class Pair(a: String, b: String)
    def unapplySeq(s: String): Option[(Pair, Seq[String])] =
      val parts = s.split("-", -1).toList
      if parts.size >= 2 then
        val pair = Pair(parts(0), parts(1))
        Some((pair, parts.drop(2)))
      else None
