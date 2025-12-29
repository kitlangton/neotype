package comptime

import util.ReflectionLookup

private[comptime] object PatternUnapplyFallback:
  def moduleOrClassFallback(
      normalized: String,
      fullName: String,
      v: Any,
      args: List[PatternIR],
      loopArgs: List[Any] => Either[ComptimeError, Option[Map[String, Any]]]
  ): Either[ComptimeError, Option[Map[String, Any]]] =
    def loopProduct(prod: Product): Either[ComptimeError, Option[Map[String, Any]]] =
      val elems = (0 until prod.productArity).toList.map(prod.productElement)
      loopArgs(elems)

    ReflectionLookup.resolveModule(normalized) match
      case Some(module) if args.isEmpty && v == module =>
        Right(Some(Map.empty))
      case _ =>
        ReflectionLookup.resolveClass(normalized) match
          case Some(cls) if cls.isInstance(v) =>
            v match
              case prod: Product => loopProduct(prod)
              case _             => if args.isEmpty then Right(Some(Map.empty)) else Right(None)
          case Some(_) =>
            Right(None)
          case None =>
            Left(ComptimeError.UnsupportedPattern("unapply", fullName))
