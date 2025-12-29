package comptime

private[comptime] object PatternMatch:
  def merge(a: Map[String, Any], b: Map[String, Any]): Option[Map[String, Any]] =
    val keysOk = a.keySet.intersect(b.keySet).forall(k => a(k) == b(k))
    if keysOk then Some(a ++ b) else None

  private val emptyBinds: Map[String, Any] = Map.empty

  def matchAll(
      patterns: List[PatternIR],
      values: List[Any],
      evalTerm: TermIR => Either[ComptimeError, Any]
  )(
      matchesFn: (PatternIR, Any, TermIR => Either[ComptimeError, Any]) => Either[ComptimeError, Option[
        Map[String, Any]
      ]]
  ): Either[ComptimeError, Option[Map[String, Any]]] =
    if patterns.size != values.size then Right(None)
    else
      @annotation.tailrec
      def loop(
          remaining: List[(Any, PatternIR)],
          acc: Map[String, Any]
      ): Either[ComptimeError, Option[Map[String, Any]]] =
        remaining match
          case Nil => Right(Some(acc))
          case (value, pat) :: tail =>
            matchesFn(pat, value, evalTerm) match
              case Left(err)   => Left(err)
              case Right(None) => Right(None)
              case Right(Some(binds)) =>
                merge(acc, binds) match
                  case Some(merged) => loop(tail, merged)
                  case None         => Right(None)

      loop(values.zip(patterns), emptyBinds)
