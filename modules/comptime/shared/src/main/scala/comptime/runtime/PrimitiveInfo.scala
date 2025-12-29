package comptime

import util.TypeNames

private[comptime] object PrimitiveInfo:
  final case class Info(
      fullName: String,
      primitiveClass: Class[?],
      boxedClass: Class[?],
      recv: Option[RecvPred]
  )

  private val infos: List[Info] = List(
    Info(TypeNames.intName, java.lang.Integer.TYPE, classOf[java.lang.Integer], Some(RuleDsl.int)),
    Info(TypeNames.longName, java.lang.Long.TYPE, classOf[java.lang.Long], Some(RuleDsl.long)),
    Info(TypeNames.floatName, java.lang.Float.TYPE, classOf[java.lang.Float], Some(RuleDsl.float)),
    Info(TypeNames.doubleName, java.lang.Double.TYPE, classOf[java.lang.Double], Some(RuleDsl.double)),
    Info(TypeNames.boolName, java.lang.Boolean.TYPE, classOf[java.lang.Boolean], Some(RuleDsl.bool)),
    Info(TypeNames.charName, java.lang.Character.TYPE, classOf[java.lang.Character], Some(RuleDsl.char)),
    Info(TypeNames.byteName, java.lang.Byte.TYPE, classOf[java.lang.Byte], Some(RuleDsl.byte)),
    Info(TypeNames.shortName, java.lang.Short.TYPE, classOf[java.lang.Short], Some(RuleDsl.short)),
    Info(TypeNames.unitName, classOf[scala.runtime.BoxedUnit], classOf[scala.runtime.BoxedUnit], None)
  )

  val classByName: Map[String, Class[?]] =
    infos.map(info => info.fullName -> info.boxedClass).toMap

  private val infoByClass: Map[Class[?], Info] =
    infos.flatMap(info => List(info.primitiveClass, info.boxedClass).distinct.map(_ -> info)).toMap

  def recvPredFor(cls: Class[?]): Option[RecvPred] =
    infoByClass.get(cls).flatMap(_.recv)
