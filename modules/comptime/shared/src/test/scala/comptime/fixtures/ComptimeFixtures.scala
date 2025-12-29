package comptime

final case class Person(name: String, age: Int)
final case class Box[A](value: A)
final case class VarBox[A](value: A, rest: A*)
case object Singleton
