package comptime

import PatternNames.*
object PatternEngine:
  def matches(
      pat: PatternIR,
      value: Any,
      evalTerm: TermIR => Either[ComptimeError, Any]
  ): Either[ComptimeError, Option[Map[String, Any]]] =
    def loop(pattern: PatternIR, v: Any): Either[ComptimeError, Option[Map[String, Any]]] =
      pattern match
        case PatternIR.Wildcard() =>
          Right(Some(Map.empty))
        case PatternIR.Literal(lit) =>
          Right(if v == lit then Some(Map.empty) else None)
        case PatternIR.Bind(name, inner) =>
          loop(inner, v).map {
            case Some(binds) => PatternMatch.merge(binds, Map(name -> v))
            case None        => None
          }
        case PatternIR.Typed(tpe, inner) =>
          if typeMatches(tpe, v) then loop(inner, v) else Right(None)
        case PatternIR.Tuple(elems) =>
          v match
            case prod: Product if prod.productArity == elems.size =>
              val values = (0 until prod.productArity).toList.map(prod.productElement)
              PatternMatch.matchAll(elems, values, evalTerm)(matches)
            case _ => Right(None)
        case PatternIR.SeqWildcard(_) =>
          Right(Some(Map.empty))
        case PatternIR.Alt(patterns) =>
          patterns.foldLeft[Either[ComptimeError, Option[Map[String, Any]]]](Right(None)) { (acc, p) =>
            acc match
              case Right(Some(binds)) => Right(Some(binds))
              case Right(None)        => loop(p, v)
              case Left(err)          => Left(err)
          }
        case PatternIR.Unapply(fullName, recv, args, isSeq) =>
          if isSeq then PatternUnapply.unapplySeqPattern(fullName, recv, v, args, evalTerm)(matches)
          else PatternUnapply.unapplyPattern(fullName, recv, v, args, evalTerm)(matches)

    loop(pat, value)

  private def typeMatches(tpe: TypeIR, value: Any): Boolean =
    tpe match
      case TypeIR.AnyType() => true
      case TypeIR.Ref(fullName, _) =>
        value match
          case cc: CaseClassValue if classNameMatches(fullName, cc.fullName) => true
          case _ =>
            if fullName == "scala.Array" then value != null && value.getClass.isArray
            else classForType(fullName).exists(_.isInstance(value))
