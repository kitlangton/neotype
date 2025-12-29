package comptime

private[comptime] object CallRuleEngine:
  // === Name-based rule index (preserves original rule order) ===
  private[comptime] final case class RuleIndex(
      byName: Map[String, Vector[CallRule]],
      anyName: Vector[CallRule]
  ):
    def candidates(name: String): Vector[CallRule] =
      byName.getOrElse(name, anyName)

  def index(rules: List[CallRule]): RuleIndex =
    val names =
      rules
        .flatMap(_.name match
          case AnyName        => Nil
          case NameIs(value)  => value :: Nil
          case NameIn(values) => values.toList)
        .toSet
    val builders       = names.map(name => name -> Vector.newBuilder[CallRule]).toMap
    val anyNameBuilder = Vector.newBuilder[CallRule]
    rules.foreach { rule =>
      rule.name match
        case AnyName =>
          anyNameBuilder += rule
          builders.values.foreach(_ += rule)
        case NameIs(value) =>
          builders.get(value).foreach(_ += rule)
        case NameIn(values) =>
          values.foreach(name => builders.get(name).foreach(_ += rule))
    }
    RuleIndex(
      byName = builders.view.mapValues(_.result()).toMap,
      anyName = anyNameBuilder.result()
    )

  // === Call dispatch ===
  // Wraps all rule compilation to ensure exceptions become Left(ComptimeError.EvalException)
  // This catches exceptions from constant folding in fold0/fold1/etc during compilation
  def compileCall(call: CallIR, ctx: RuleContext, index: RuleIndex): Either[ComptimeError, Eval] =
    try
      if ComptimeDebug.enabled then ComptimeDebug.log(s"[comptime] call: ${formatCall(call)}")
      val matches = index.candidates(call.name).filter(rule => CallMatcher.matchesCall(rule, call)).toList
      matches match
        case rule :: _ =>
          if ComptimeDebug.enabled then ComptimeDebug.log(s"[comptime] rule: ${rule.id}")
          rule.compile(call, ctx) match
            case Left(err) =>
              if ComptimeDebug.enabled then ComptimeDebug.log(s"[comptime] rule failed: ${ComptimeError.format(err)}")
              Left(err)
            case right => right
        case Nil =>
          compileAccessor(call, ctx).getOrElse {
            val details =
              if ComptimeDebug.enabled then
                Map(
                  "call"  -> formatCall(call),
                  "owner" -> call.owner,
                  "name"  -> call.name,
                  "arity" -> call.args.map(_.size).mkString("[", ",", "]"),
                  "rules" -> index.candidates(call.name).map(_.id).mkString(", ")
                )
              else Map.empty
            Left(ComptimeError.UnsupportedCall(call.owner, call.name, details))
          }
    catch
      case e: ComptimeAbort =>
        Left(ComptimeError.UserAbort(e.message))
      case e: Throwable =>
        val ctx = s"${shortenOwner(call.owner)}.${call.name}"
        Left(ComptimeError.EvalException(e.getClass.getSimpleName, e.getMessage, Some(ctx), call.pos))

  private def shortenOwner(owner: String): String =
    owner.split('.').lastOption.getOrElse(owner).stripSuffix("$")

  private def compileAccessor(call: CallIR, ctx: RuleContext): Option[Either[ComptimeError, Eval]] =
    if !call.args0 then None
    else
      def accessorValue(value: Any, name: String): Option[Any] =
        value match
          case cc: CaseClassValue =>
            cc.fields.find(_._1 == name).map(_._2)
          case prod: Product =>
            val idxOpt =
              (0 until prod.productArity).find { i =>
                try prod.productElementName(i) == name
                catch case _: Throwable => false
              }
            idxOpt.map(prod.productElement).orElse(reflectGetter(value, name))
          case _ =>
            reflectGetter(value, name)

      def reflectGetter(value: Any, name: String): Option[Any] =
        if value == null then None
        else
          try
            val method = value.getClass.getMethod(name)
            if method.getParameterCount == 0 then Some(method.invoke(value))
            else None
          catch case _: Throwable => None

      Some(
        ctx.compileTerm(call.recv).flatMap { recvEval =>
          recvEval match
            case Eval.Value(value) =>
              accessorValue(value, call.name) match
                case Some(result) => Right(Eval.Value(result))
                case None         => Left(ComptimeError.UnsupportedCall(call.owner, call.name))
            case _ =>
              Right(
                Eval.Apply1(
                  recvEval,
                  Eval.Value(()),
                  (a: Any, _: Any) =>
                    accessorValue(a, call.name).getOrElse {
                      throw new RuntimeException(s"Missing accessor: ${call.name}")
                    }
                )
              )
        }
      )

  def formatCall(call: CallIR): String =
    val argSizes = call.args.map(_.size).mkString("(", ",", ")")
    val targs    = if call.targs.isEmpty then "" else call.targs.mkString("[", ",", "]")
    s"${call.owner}.${call.name}$targs arity=$argSizes"

// === Match predicates ===
private[comptime] object CallMatcher:
  def matchesCall(rule: CallRule, call: CallIR): Boolean =
    matchesName(rule.name, call.name) && matchesRecv(rule.recv, call) && matchesArity(rule.arity, call)

  private def matchesName(pred: NamePred, name: String): Boolean = pred match
    case AnyName        => true
    case NameIs(value)  => name == value
    case NameIn(values) => values.contains(name)

  private def matchesRecv(pred: RecvPred, call: CallIR): Boolean =
    def stripModule(name: String): String =
      util.TypeNames.stripModule(name)
    def matchesOwner(owner: String): Boolean =
      pred match
        case AnyRecv => true
        case TypeRecv(fullName) =>
          owner == fullName ||
          (owner.endsWith("$") && stripModule(owner) == stripModule(fullName))
        case UnionRecv(names) =>
          names.contains(owner) ||
          (owner.endsWith("$") && names.exists(n => stripModule(n) == stripModule(owner)))
    def matchesRecvRef: Boolean =
      call.recv match
        case TermIR.Ref(_, Some(fullName)) =>
          matchesOwner(fullName) ||
          (fullName.endsWith("$") && matchesOwner(stripModule(fullName)))
        case _ => false
    matchesOwner(call.owner) || matchesRecvRef

  private def matchesArity(arity: Arity, call: CallIR): Boolean =
    val normalized = call.args.filter(_.nonEmpty)
    def arityOf(argss: List[List[TermIR]]): Arity =
      argss match
        case Nil                             => A0
        case List(List(_))                   => A1
        case List(List(_, _))                => A2
        case List(List(_, _, _))             => A3
        case List(List(_, _, _, _))          => A4
        case List(List(_, _, _, _, _))       => A5
        case List(List(_), List(_))          => A1_1
        case List(List(_), List(_), List(_)) => A1_1_1
        case List(List(_), List(_, _))       => A1_2
        case _                               => ASet(Set.empty)
    arity match
      case ASet(values) if values.nonEmpty => values.contains(arityOf(normalized))
      case ASet(values) if values.isEmpty  => true
      case other                           => other == arityOf(normalized)
