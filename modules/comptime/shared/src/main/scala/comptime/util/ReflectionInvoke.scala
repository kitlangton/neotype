package comptime.util

import java.lang.reflect.Constructor
import java.lang.reflect.Method

object ReflectionInvoke:
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

  def pickApply(target: AnyRef, argValues: List[Any]): Option[(Method, List[Any])] =
    pickMethod(target.getClass, "apply", argValues)

  def pickMethod(
      targetClass: Class[?],
      name: String,
      argValues: List[Any]
  ): Option[(Method, List[Any])] =
    val methods = targetClass.getMethods.filter(_.getName == name)

    def exactMatch: Option[(Method, List[Any])] =
      methods
        .find { m =>
          m.getParameterCount == argValues.size &&
          m.getParameterTypes.toList.zip(argValues).forall { case (p, v) => paramOk(p, v) }
        }
        .map(m => (m, argValues))

    def varargMatch: Option[(Method, List[Any])] =
      methods.view
        .filter(m => m.getParameterCount >= 1 && isSeqParam(m.getParameterTypes.last))
        .flatMap { m =>
          val fixedCount = m.getParameterCount - 1
          if argValues.size < fixedCount then None
          else
            val fixed   = argValues.take(fixedCount)
            val rest    = argValues.drop(fixedCount)
            val fixedOk = m.getParameterTypes.take(fixedCount).toList.zip(fixed).forall { case (p, v) => paramOk(p, v) }
            if fixedOk then Some((m, fixed :+ rest.toList)) else None
        }
        .headOption

    exactMatch.orElse(varargMatch)

  def pickConstructor(
      targetClass: Class[?],
      argValues: List[Any]
  ): Option[(Constructor[?], List[Any])] =
    val ctors = targetClass.getConstructors.toList

    def exactMatch: Option[(Constructor[?], List[Any])] =
      ctors
        .find { c =>
          c.getParameterCount == argValues.size &&
          c.getParameterTypes.toList.zip(argValues).forall { case (p, v) => paramOk(p, v) }
        }
        .map(c => (c, argValues))

    def varargMatch: Option[(Constructor[?], List[Any])] =
      ctors.view
        .filter(c => c.getParameterCount >= 1 && isSeqParam(c.getParameterTypes.last))
        .flatMap { c =>
          val fixedCount = c.getParameterCount - 1
          if argValues.size < fixedCount then None
          else
            val fixed   = argValues.take(fixedCount)
            val rest    = argValues.drop(fixedCount)
            val fixedOk = c.getParameterTypes.take(fixedCount).toList.zip(fixed).forall { case (p, v) => paramOk(p, v) }
            if fixedOk then Some((c, fixed :+ rest.toList)) else None
        }
        .headOption

    exactMatch.orElse(varargMatch)
