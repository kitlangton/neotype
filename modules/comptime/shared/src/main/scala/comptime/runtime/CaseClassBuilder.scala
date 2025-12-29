package comptime

import util.ReflectionLookup

private[comptime] object CaseClassBuilder:
  def build(
      fullName: String,
      fields: List[String],
      repeatedIndex: Option[Int],
      values: List[Any]
  ): Any =
    def adjustedValues: List[Any] =
      repeatedIndex match
        case Some(idx) if idx >= 0 && idx < fields.length =>
          val (prefix, rest) = values.splitAt(idx)
          prefix :+ rest.toList
        case _ =>
          if fields.nonEmpty && values.size > fields.length then
            val (prefix, rest) = values.splitAt(fields.length - 1)
            prefix :+ rest.toList
          else values

    def caseClassValue: CaseClassValue =
      CaseClassValue(fullName, fields.zip(adjustedValues).toVector)

    def paramOk(param: Class[?], value: Any): Boolean =
      if value == null then !param.isPrimitive
      else if param.isPrimitive then
        val boxed =
          param match
            case java.lang.Integer.TYPE   => classOf[java.lang.Integer]
            case java.lang.Long.TYPE      => classOf[java.lang.Long]
            case java.lang.Double.TYPE    => classOf[java.lang.Double]
            case java.lang.Boolean.TYPE   => classOf[java.lang.Boolean]
            case java.lang.Character.TYPE => classOf[java.lang.Character]
            case java.lang.Byte.TYPE      => classOf[java.lang.Byte]
            case java.lang.Short.TYPE     => classOf[java.lang.Short]
            case java.lang.Float.TYPE     => classOf[java.lang.Float]
            case java.lang.Void.TYPE      => classOf[scala.runtime.BoxedUnit]
            case _                        => param
        boxed.isInstance(value)
      else param.isInstance(value)

    def isSeqParam(param: Class[?]): Boolean =
      classOf[scala.collection.Seq[?]].isAssignableFrom(param) ||
        classOf[scala.collection.Iterable[?]].isAssignableFrom(param)

    def pickConstructor(
        targetClass: Class[?],
        argValues: List[Any]
    ): Option[(java.lang.reflect.Constructor[?], List[Any])] =
      val ctors = targetClass.getConstructors.toList
      def exactMatch: Option[(java.lang.reflect.Constructor[?], List[Any])] =
        ctors
          .find { c =>
            c.getParameterCount == argValues.size &&
            c.getParameterTypes.toList.zip(argValues).forall { case (p, v) => paramOk(p, v) }
          }
          .map(c => (c, argValues))

      def varargMatch: Option[(java.lang.reflect.Constructor[?], List[Any])] =
        ctors.view
          .filter(c => c.getParameterCount >= 1 && isSeqParam(c.getParameterTypes.last))
          .flatMap { c =>
            val fixedCount = c.getParameterCount - 1
            if argValues.size < fixedCount then None
            else
              val fixed = argValues.take(fixedCount)
              val rest  = argValues.drop(fixedCount)
              val fixedOk =
                c.getParameterTypes.take(fixedCount).toList.zip(fixed).forall { case (p, v) => paramOk(p, v) }
              if fixedOk then Some((c, fixed :+ rest.toList)) else None
          }
          .headOption

      exactMatch.orElse(varargMatch)

    val ctorArgs = adjustedValues
    ReflectionLookup.resolveClass(fullName) match
      case Some(cls) =>
        pickConstructor(cls, ctorArgs) match
          case Some((ctor, invokeArgs)) =>
            try ctor.newInstance(invokeArgs.map(_.asInstanceOf[AnyRef])*)
            catch case _: Throwable => caseClassValue
          case None =>
            caseClassValue
      case None =>
        caseClassValue
