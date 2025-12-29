package comptime

import util.ReflectionLookup

object ReferenceResolver:
  val globalRefs: Map[String, Eval] = Map(
    "None"            -> Eval.Value(None),
    "Nil"             -> Eval.Value(Nil),
    "$conforms"       -> Eval.Value(scala.Predef.$conforms[Any]),
    "refl"            -> Eval.Value((x: Any) => x),
    "intWrapper"      -> Eval.Value((i: Int) => new scala.runtime.RichInt(i)),
    "option2Iterable" -> Eval.Value((opt: Option[Any]) => opt.toList),
    "augmentString"   -> Eval.Value((s: String) => s),
    "wrapString"      -> Eval.Value((s: String) => s),
    "charWrapper"     -> Eval.Value((c: Char) => c),
    "byteWrapper"     -> Eval.Value((b: Byte) => b),
    "shortWrapper"    -> Eval.Value((s: Short) => s),
    "floatWrapper"    -> Eval.Value((f: Float) => f),
    "wrapRefArray"    -> Eval.Value((arr: Array[AnyRef]) => arr.toSeq)
  )

  def resolveRef(name: String, fullName: Option[String]): Option[Eval] =
    globalRefs
      .get(name)
      .orElse(fullName.flatMap(resolveModule))
      .orElse(fullName.flatMap(resolveEnumCase))
      .orElse(fullName.flatMap(resolveClass))
      .orElse(resolveModuleByName(name))

  def resolveEnumCase(fullName: String): Option[Eval] =
    ReflectionLookup.resolveEnumCase(fullName).map(v => Eval.Value(v))

  def resolveOrError(name: String, fullName: Option[String], env: Map[String, Eval]): Either[ComptimeError, Eval] =
    env
      .get(name)
      .orElse(resolveRef(name, fullName))
      .toRight {
        val details =
          if ComptimeDebug.enabled then
            Map(
              "name"     -> name,
              "fullName" -> fullName.getOrElse("<none>"),
              "env"      -> env.keys.toList.sorted.mkString(", "),
              "globals"  -> globalRefs.keys.toList.sorted.mkString(", ")
            )
          else Map.empty
        ComptimeFailure.UnresolvedReference(name, fullName, details.get("env").map(_.split(", ").toList).getOrElse(Nil))
      }

  def resolveModule(fullName: String): Option[Eval] =
    val aliases = fullName match
      case "scala.Either" => List("scala.util.Either")
      case "scala.Left"   => List("scala.util.Left")
      case "scala.Right"  => List("scala.util.Right")
      case _              => Nil
    val candidates = (fullName :: aliases).distinct
    candidates.view.flatMap { name =>
      ReflectionLookup.resolveModule(name).map(v => Eval.Value(v))
    }.headOption

  def resolveClass(fullName: String): Option[Eval] =
    ReflectionLookup.resolveClass(fullName).map(cls => Eval.Value(cls))

  def resolveModuleByName(name: String): Option[Eval] =
    if name.nonEmpty && name.head.isUpper then
      val candidates = List(
        s"scala.util.$name",
        s"scala.$name",
        s"scala.math.$name",
        s"scala.collection.$name",
        s"scala.collection.immutable.$name",
        s"java.time.$name"
      )
      candidates.view.flatMap(resolveModule).headOption
    else None
