package comptime

import PatternNames.*

private[comptime] object PatternUnapplySpecs:
  final case class UnapplySpec(
      names: Set[String],
      handle: (Any, List[PatternIR], List[Any] => Either[ComptimeError, Option[Map[String, Any]]]) => Either[
        ComptimeError,
        Option[Map[String, Any]]
      ]
  )

  final case class UnapplySeqSpec(
      names: Set[String],
      values: Any => Option[PatternUnapplySeqMatch.SeqValues]
  )

  private val emptyBinds: Map[String, Any] = Map.empty

  private val simpleSpecs: List[UnapplySpec] = List(
    UnapplySpec(
      optionNames,
      (value, _, loopArgs) =>
        value match
          case Some(x) => loopArgs(List(x))
          case _       => Right(None)
    ),
    UnapplySpec(
      noneNames,
      (value, args, _) => Right(if value == None && args.isEmpty then Some(emptyBinds) else None)
    ),
    UnapplySpec(
      leftNames,
      (value, _, loopArgs) =>
        value match
          case Left(x) => loopArgs(List(x))
          case _       => Right(None)
    ),
    UnapplySpec(
      rightNames,
      (value, _, loopArgs) =>
        value match
          case Right(x) => loopArgs(List(x))
          case _        => Right(None)
    ),
    UnapplySpec(
      successNames,
      (value, _, loopArgs) =>
        value match
          case scala.util.Success(x) => loopArgs(List(x))
          case _                     => Right(None)
    ),
    UnapplySpec(
      failureNames,
      (value, _, loopArgs) =>
        value match
          case scala.util.Failure(x) => loopArgs(List(x))
          case _                     => Right(None)
    ),
    UnapplySpec(
      seqNames,
      (value, _, loopArgs) =>
        value match
          case xs: Seq[?] => loopArgs(xs.toList)
          case _          => Right(None)
    ),
    UnapplySpec(
      consNames,
      (value, _, loopArgs) =>
        value match
          case h :: t => loopArgs(List(h, t))
          case _      => Right(None)
    ),
    UnapplySpec(
      nilNames,
      (value, args, _) => Right(if value == Nil && args.isEmpty then Some(emptyBinds) else None)
    )
  )

  private val seqSpecs: List[UnapplySeqSpec] = List(
    UnapplySeqSpec(
      seqUnapplyNames,
      PatternUnapplySeqMatch.valuesFromUnapplySeq
    )
  )
  private val simpleSpecCandidates: Map[String, List[UnapplySpec]] =
    simpleSpecs.flatMap(spec => spec.names.map(_ -> spec)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  private val seqSpecCandidates: Map[String, List[UnapplySeqSpec]] =
    seqSpecs.flatMap(spec => spec.names.map(_ -> spec)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  def matchSimple(
      normalized: String,
      args: List[PatternIR],
      value: Any,
      loopArgs: List[Any] => Either[ComptimeError, Option[Map[String, Any]]]
  ): Option[Either[ComptimeError, Option[Map[String, Any]]]] =
    val candidates = simpleSpecCandidates.getOrElse(normalized, Nil)
    candidates.collectFirst {
      case spec if nameMatches(normalized, spec.names) => spec.handle(value, args, loopArgs)
    }

  def matchSeq(normalized: String, value: Any): Option[PatternUnapplySeqMatch.SeqValues] =
    val candidates = seqSpecCandidates.getOrElse(normalized, Nil)
    candidates.collectFirst {
      case spec if nameMatches(normalized, spec.names) => spec.values(value)
    }.flatten
