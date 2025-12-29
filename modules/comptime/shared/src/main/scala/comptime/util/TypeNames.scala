package comptime.util

object TypeNames:
  def stripModule(name: String): String =
    if name.endsWith("$") then name.dropRight(1) else name

  val intName    = "scala.Int"
  val longName   = "scala.Long"
  val floatName  = "scala.Float"
  val doubleName = "scala.Double"
  val boolName   = "scala.Boolean"
  val charName   = "scala.Char"
  val byteName   = "scala.Byte"
  val shortName  = "scala.Short"
  val unitName   = "scala.Unit"

  val stringClassNames: Set[String] = Set(
    "java.lang.String",
    "scala.Predef.String",
    "scala.Predef$.String",
    "scala.String"
  )

  val stringReceivers: Set[String] =
    stringClassNames + "scala.collection.StringOps" + "scala.collection.immutable.WrappedString"

  val consNames: Set[String] = Set(
    "scala.::",
    "scala.::$",
    "scala.collection.immutable.::",
    "scala.collection.immutable.::$",
    "scala.collection.immutable.$colon$colon",
    "scala.collection.immutable.$colon$colon$"
  )

  val optionNames: Set[String]  = Set("scala.Some", "scala.Option")
  val noneNames: Set[String]    = Set("scala.None")
  val leftNames: Set[String]    = Set("scala.util.Left", "scala.util.Either")
  val rightNames: Set[String]   = Set("scala.util.Right")
  val successNames: Set[String] = Set("scala.util.Success")
  val failureNames: Set[String] = Set("scala.util.Failure")
  val seqNames: Set[String] = Set(
    "scala.collection.immutable.List",
    "scala.collection.immutable.Vector",
    "scala.collection.immutable.Seq",
    "scala.collection.Seq"
  )
  val nilNames: Set[String] = Set(
    "scala.collection.immutable.Nil",
    "scala.Nil",
    "scala.package$.Nil"
  )
  val seqUnapplyNames: Set[String] = Set(
    "scala.collection.immutable.List",
    "scala.collection.immutable.Vector",
    "scala.collection.immutable.Seq",
    "scala.collection.Seq",
    "scala.collection.SeqFactory",
    "scala.collection.immutable.SeqFactory",
    "scala.Array"
  )
