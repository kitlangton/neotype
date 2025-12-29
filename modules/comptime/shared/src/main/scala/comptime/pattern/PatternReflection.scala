package comptime

import util.ReflectionLookup

private[comptime] object PatternReflection:
  private val primitiveToBoxed: Map[Class[?], Class[?]] = Map(
    java.lang.Integer.TYPE   -> classOf[java.lang.Integer],
    java.lang.Long.TYPE      -> classOf[java.lang.Long],
    java.lang.Double.TYPE    -> classOf[java.lang.Double],
    java.lang.Boolean.TYPE   -> classOf[java.lang.Boolean],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Byte.TYPE      -> classOf[java.lang.Byte],
    java.lang.Short.TYPE     -> classOf[java.lang.Short],
    java.lang.Float.TYPE     -> classOf[java.lang.Float],
    java.lang.Void.TYPE      -> classOf[scala.runtime.BoxedUnit]
  )
  private val methodCache =
    scala.collection.concurrent.TrieMap.empty[(Class[?], String, Option[Class[?]]), Option[java.lang.reflect.Method]]

  private def boxedType(param: Class[?]): Class[?] =
    if param.isPrimitive then primitiveToBoxed.getOrElse(param, param) else param

  private def typeDistance(param: Class[?], argClass: Class[?]): Option[Int] =
    val boxed = boxedType(param)
    if !boxed.isAssignableFrom(argClass) then None
    else if boxed == argClass then Some(0)
    else
      val visited            = scala.collection.mutable.Set[Class[?]](argClass)
      val queue              = scala.collection.mutable.Queue[(Class[?], Int)]((argClass, 0))
      var found: Option[Int] = None
      while queue.nonEmpty && found.isEmpty do
        val (current, dist) = queue.dequeue()
        if boxed == current then found = Some(dist)
        else
          val supers = Option(current.getSuperclass).toList ++ current.getInterfaces.toList
          supers.filterNot(visited.contains).foreach { sup =>
            visited += sup
            queue.enqueue((sup, dist + 1))
          }
      found.orElse(Some(1))

  private def pickMethod(target: AnyRef, name: String, arg: Any): Option[java.lang.reflect.Method] =
    val argClassOpt = Option(arg).map(_.getClass)
    methodCache.getOrElseUpdate(
      (target.getClass, name, argClassOpt), {
        val methods = target.getClass.getMethods.filter(m => m.getName == name && m.getParameterCount == 1)
        argClassOpt match
          case None =>
            methods.find(m => !m.getParameterTypes.head.isPrimitive)
          case Some(argClass) =>
            val scored = methods.flatMap { m =>
              typeDistance(m.getParameterTypes.head, argClass).map(score => (m, score))
            }
            scored.sortBy(_._2).headOption.map(_._1)
      }
    )

  private def invokeOnTarget(
      target: AnyRef,
      method: String,
      arg: Any,
      missingError: => ComptimeError,
      throwLabel: String
  ): Either[ComptimeError, Option[Any]] =
    pickMethod(target, method, arg) match
      case None => Left(missingError)
      case Some(m) =>
        try Right(Some(m.invoke(target, arg.asInstanceOf[AnyRef])))
        catch
          case e: Throwable =>
            val cause = Option(e.getCause).getOrElse(e)
            Left(
              ComptimeFailure.UnsupportedPattern(
                "unapply",
                s"$throwLabel.$method threw: ${cause.toString}"
              )
            )

  def invokeModuleMethod(fullName: String, method: String, arg: Any): Either[ComptimeError, Option[Any]] =
    ReflectionLookup.resolveModule(fullName) match
      case None => Right(None)
      case Some(module) =>
        invokeOnTarget(
          module,
          method,
          arg,
          missingError = ComptimeFailure.UnsupportedPattern("unapply", s"$fullName.$method"),
          throwLabel = fullName
        )

  def invokeInstanceMethod(
      recv: TermIR,
      method: String,
      arg: Any,
      evalTerm: TermIR => Either[ComptimeError, Any]
  ): Either[ComptimeError, Option[Any]] =
    evalTerm(recv).flatMap {
      case target: AnyRef =>
        invokeOnTarget(
          target,
          method,
          arg,
          missingError = ComptimeFailure.UnsupportedPattern("unapply", s"${target.getClass.getName}.$method"),
          throwLabel = target.getClass.getName
        )
      case other =>
        Left(ComptimeFailure.UnsupportedPattern("unapply", s"receiver: ${other.getClass.getName}"))
    }
