package comptime.util

private[comptime] object ReflectionLookup:
  private val moduleCache = scala.collection.concurrent.TrieMap.empty[String, Option[AnyRef]]
  private val classCache  = scala.collection.concurrent.TrieMap.empty[String, Option[Class[?]]]

  def candidateNames(fullName: String): List[String] =
    val normalized = fullName.replace("$.", "$")
    val base       = if normalized.endsWith("$") then normalized.dropRight(1) else normalized
    val dotIdx     = base.indices.filter(i => base.charAt(i) == '.').toList
    (0 to dotIdx.size).toList.map { k =>
      if k == 0 then base
      else
        val replace = dotIdx.takeRight(k).toSet
        base.zipWithIndex.map { case (ch, idx) => if ch == '.' && replace.contains(idx) then '$' else ch }.mkString
    }.distinct

  def resolveModule(fullName: String): Option[AnyRef] =
    moduleCache.getOrElseUpdate(
      fullName,
      candidateNames(fullName).view.flatMap { name =>
        val className = if name.endsWith("$") then name else name + "$"
        try
          val cls   = Class.forName(className)
          val field = cls.getField("MODULE$")
          Option(field.get(null).asInstanceOf[AnyRef])
        catch case _: Throwable => None
      }.headOption
    )

  def resolveClass(fullName: String): Option[Class[?]] =
    classCache.getOrElseUpdate(
      fullName,
      candidateNames(fullName).view.flatMap { name =>
        val className = if name.endsWith("$") then name.dropRight(1) else name
        try Some(Class.forName(className))
        catch case _: Throwable => None
      }.headOption
    )

  /** Resolve an enum case like "comptime.Row.Top" or "comptime.Row$.Top" by
    * loading the companion object and calling valueOf(caseName). Also supports
    * Java enums like "java.time.Month.MARCH".
    */
  def resolveEnumCase(fullName: String): Option[AnyRef] =
    // Split "comptime.Row.Top" or "comptime.Row$.Top" into (enumClass, caseName)
    val lastDot = fullName.lastIndexOf('.')
    if lastDot <= 0 then None
    else
      val rawEnumClass = fullName.substring(0, lastDot)
      // Remove trailing $ if present (companion object reference)
      val enumClassName = if rawEnumClass.endsWith("$") then rawEnumClass.dropRight(1) else rawEnumClass
      val caseName      = fullName.substring(lastDot + 1)

      // Try Scala 3 enum first (companion with valueOf)
      val scalaEnumResult = candidateNames(enumClassName).view.flatMap { name =>
        val companionName = if name.endsWith("$") then name else name + "$"
        try
          val companionCls  = Class.forName(companionName)
          val moduleField   = companionCls.getField("MODULE$")
          val companion     = moduleField.get(null)
          val valueOfMethod = companionCls.getMethod("valueOf", classOf[String])
          val result        = valueOfMethod.invoke(companion, caseName)
          Option(result.asInstanceOf[AnyRef])
        catch case _: Throwable => None
      }.headOption

      // If Scala enum fails, try Java enum (static field on enum class)
      scalaEnumResult.orElse {
        candidateNames(enumClassName).view.flatMap { name =>
          try
            val enumCls = Class.forName(name)
            if enumCls.isEnum then
              val field = enumCls.getField(caseName)
              Option(field.get(null).asInstanceOf[AnyRef])
            else None
          catch case _: Throwable => None
        }.headOption
      }
