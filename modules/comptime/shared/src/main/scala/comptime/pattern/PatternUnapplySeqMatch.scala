package comptime

private[comptime] object PatternUnapplySeqMatch:
  final case class SeqValues(values: List[Any], fixedPrefix: Int)
  private val seqValuesCache = scala.collection.concurrent.TrieMap.empty[AnyRef, Option[SeqValues]]

  def matchSeqValues(
      args: List[PatternIR],
      values: List[Any],
      fixedPrefix: Int,
      evalTerm: TermIR => Either[ComptimeError, Any],
      matchesFn: (PatternIR, Any, TermIR => Either[ComptimeError, Any]) => Either[ComptimeError, Option[
        Map[String, Any]
      ]]
  ): Either[ComptimeError, Option[Map[String, Any]]] =
    def splitSeqArgs: (List[PatternIR], Boolean, Option[String]) =
      args match
        case init :+ PatternIR.SeqWildcard(bind) => (init, true, bind)
        case _                                   => (args, false, None)

    def matchPrefix(values: List[Any], patterns: List[PatternIR]): Either[ComptimeError, Option[Map[String, Any]]] =
      PatternMatch.matchAll(patterns, values, evalTerm)(matchesFn)

    val (prefix, hasStar, starBind) = splitSeqArgs
    if !hasStar then
      if values.size != prefix.size then Right(None)
      else matchPrefix(values, prefix)
    else if values.size < prefix.size then Right(None)
    else if fixedPrefix > 0 && prefix.size <= fixedPrefix then
      val fixedVals  = values.take(fixedPrefix)
      val seqVals    = values.drop(fixedPrefix)
      val prefixVals = fixedVals.take(prefix.size)
      matchPrefix(prefixVals, prefix).map {
        case Some(binds) =>
          val bindVal = seqVals.toList
          val bindMap = starBind.map(name => Map(name -> bindVal)).getOrElse(Map.empty)
          PatternMatch.merge(binds, bindMap)
        case None => None
      }
    else
      val (headVals, tailVals) = values.splitAt(prefix.size)
      matchPrefix(headVals, prefix).map {
        case Some(binds) =>
          val bindVal = tailVals.toList
          val bindMap = starBind.map(name => Map(name -> bindVal)).getOrElse(Map.empty)
          PatternMatch.merge(binds, bindMap)
        case None => None
      }

  def valuesFromUnapplySeq(value: Any): Option[SeqValues] =
    value match
      case ref: AnyRef =>
        seqValuesCache.getOrElseUpdate(ref, computeValues(value))
      case _ =>
        computeValues(value)

  private def computeValues(value: Any): Option[SeqValues] =
    value match
      case xs: Seq[?]    => Some(SeqValues(xs.toList, fixedPrefix = 0))
      case arr: Array[?] => Some(SeqValues(arr.toList, fixedPrefix = 0))
      case prod: Product =>
        val elems = (0 until prod.productArity).toList.map(prod.productElement)
        elems.lastOption match
          case Some(seq: Seq[?]) =>
            val fixed = elems.dropRight(1)
            Some(SeqValues(fixed ++ seq.toList, fixedPrefix = fixed.size))
          case Some(arr: Array[?]) =>
            val fixed = elems.dropRight(1)
            Some(SeqValues(fixed ++ arr.toList, fixedPrefix = fixed.size))
          case _ =>
            Some(SeqValues(elems, fixedPrefix = 0))
      case _ => None
