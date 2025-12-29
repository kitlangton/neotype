package comptime

import util.ReflectionLookup
import util.TypeNames

private[comptime] object PatternNames:
  // Scala package object type aliases to Java classes
  private val scalaPackageAliases: Map[String, String] = Map(
    // scala.package$ aliases (used when exception is referenced via scala.package object)
    "scala.package$.NumberFormatException"           -> "java.lang.NumberFormatException",
    "scala.package$.ArithmeticException"             -> "java.lang.ArithmeticException",
    "scala.package$.IllegalArgumentException"        -> "java.lang.IllegalArgumentException",
    "scala.package$.IllegalStateException"           -> "java.lang.IllegalStateException",
    "scala.package$.NullPointerException"            -> "java.lang.NullPointerException",
    "scala.package$.IndexOutOfBoundsException"       -> "java.lang.IndexOutOfBoundsException",
    "scala.package$.ArrayIndexOutOfBoundsException"  -> "java.lang.ArrayIndexOutOfBoundsException",
    "scala.package$.StringIndexOutOfBoundsException" -> "java.lang.StringIndexOutOfBoundsException",
    "scala.package$.NoSuchElementException"          -> "java.util.NoSuchElementException",
    "scala.package$.UnsupportedOperationException"   -> "java.lang.UnsupportedOperationException",
    "scala.package$.RuntimeException"                -> "java.lang.RuntimeException",
    "scala.package$.Exception"                       -> "java.lang.Exception",
    "scala.package$.Throwable"                       -> "java.lang.Throwable",
    "scala.package$.Error"                           -> "java.lang.Error",
    // Direct scala.* aliases
    "scala.NumberFormatException"           -> "java.lang.NumberFormatException",
    "scala.ArithmeticException"             -> "java.lang.ArithmeticException",
    "scala.IllegalArgumentException"        -> "java.lang.IllegalArgumentException",
    "scala.IllegalStateException"           -> "java.lang.IllegalStateException",
    "scala.NullPointerException"            -> "java.lang.NullPointerException",
    "scala.IndexOutOfBoundsException"       -> "java.lang.IndexOutOfBoundsException",
    "scala.ArrayIndexOutOfBoundsException"  -> "java.lang.ArrayIndexOutOfBoundsException",
    "scala.StringIndexOutOfBoundsException" -> "java.lang.StringIndexOutOfBoundsException",
    "scala.NoSuchElementException"          -> "java.util.NoSuchElementException",
    "scala.UnsupportedOperationException"   -> "java.lang.UnsupportedOperationException",
    "scala.RuntimeException"                -> "java.lang.RuntimeException",
    "scala.Exception"                       -> "java.lang.Exception",
    "scala.Throwable"                       -> "java.lang.Throwable",
    "scala.Error"                           -> "java.lang.Error"
  )

  def resolveScalaAlias(fullName: String): String =
    scalaPackageAliases.getOrElse(fullName, fullName)

  def stripModule(fullName: String): String =
    if fullName.endsWith("$") then fullName.dropRight(1) else fullName

  def stripExtractor(fullName: String): String =
    if fullName.endsWith(".unapplySeq") then fullName.dropRight(".unapplySeq".length)
    else if fullName.endsWith(".unapply") then fullName.dropRight(".unapply".length)
    else if fullName.endsWith(".apply") then fullName.dropRight(".apply".length)
    else fullName

  def nameMatches(fullName: String, names: Set[String]): Boolean =
    val stripped = stripModule(stripExtractor(fullName))
    names.exists(name => stripModule(stripExtractor(name)) == stripped)

  def classNameMatches(a: String, b: String): Boolean =
    val aNames = ReflectionLookup.candidateNames(stripExtractor(a)).map(stripModule).toSet
    val bNames = ReflectionLookup.candidateNames(stripExtractor(b)).map(stripModule).toSet
    aNames.intersect(bNames).nonEmpty

  val optionNames: Set[String]     = TypeNames.optionNames
  val noneNames: Set[String]       = TypeNames.noneNames
  val leftNames: Set[String]       = TypeNames.leftNames
  val rightNames: Set[String]      = TypeNames.rightNames
  val successNames: Set[String]    = TypeNames.successNames
  val failureNames: Set[String]    = TypeNames.failureNames
  val seqNames: Set[String]        = TypeNames.seqNames
  val consNames: Set[String]       = TypeNames.consNames
  val nilNames: Set[String]        = TypeNames.nilNames
  val seqUnapplyNames: Set[String] = TypeNames.seqUnapplyNames

  private val directTypeClasses: Map[String, Class[?]] =
    TypeNames.stringClassNames.map(_ -> classOf[java.lang.String]).toMap ++
      PrimitiveInfo.classByName

  def classForType(fullName: String): Option[Class[?]] =
    val normalized = stripExtractor(fullName)
    val resolved   = resolveScalaAlias(normalized)
    directTypeClasses.get(resolved).orElse {
      if TypeNames.consNames.contains(fullName) then
        try Some(Class.forName("scala.collection.immutable.$colon$colon"))
        catch case _: Throwable => None
      else ReflectionLookup.resolveClass(resolved)
    }
