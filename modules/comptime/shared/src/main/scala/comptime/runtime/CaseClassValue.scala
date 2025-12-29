package comptime

private[comptime] final case class CaseClassValue(fullName: String, fields: Vector[(String, Any)]) extends Product:
  override def canEqual(that: Any): Boolean =
    that match
      case CaseClassValue(otherName, _) => otherName == fullName
      case _                            => false

  override def productArity: Int = fields.length

  override def productElement(n: Int): Any = fields(n)._2

  override def productPrefix: String =
    val idx = fullName.lastIndexOf('.')
    if idx >= 0 then fullName.substring(idx + 1) else fullName

  def field(name: String): Option[Any] =
    fields.find(_._1 == name).map(_._2)

  override def equals(other: Any): Boolean =
    other match
      case CaseClassValue(otherName, otherFields) =>
        otherName == fullName && fields == otherFields
      case _ => false

  override def hashCode(): Int =
    31 * fullName.hashCode + fields.hashCode
