package comptime

extension (call: CallIR)
  private def compileRecvN(
      ctx: RuleContext,
      name: String,
      expectedArgs: Int,
      argsOpt: Option[List[TermIR]],
      compileArg: TermIR => Either[ComptimeError, Eval]
  )(
      f: (Eval, List[Eval]) => Eval
  ): Either[ComptimeError, Eval] =
    def stripModule(name: String): String =
      util.TypeNames.stripModule(name)
    def extensionReceiver: Option[(TermIR, List[TermIR])] =
      call.recv match
        case TermIR.Ref(_, Some(fullName)) if stripModule(fullName) == stripModule(call.owner) =>
          val flatArgs = call.args.flatten
          if flatArgs.size == expectedArgs + 1 then Some((flatArgs.head, flatArgs.tail))
          else None
        case _ => None

    extensionReceiver match
      case Some((recvTerm, argTerms)) =>
        for
          recvEval <- ctx.compileTerm(recvTerm)
          argEvals <- util.Args.compileArgsList(argTerms, compileArg)
        yield f(recvEval, argEvals)
      case None =>
        argsOpt match
          case Some(args) =>
            for
              recvEval <- ctx.compileTerm(call.recv)
              argEvals <- util.Args.compileArgsList(args, compileArg)
            yield f(recvEval, argEvals)
          case None =>
            call.recv match
              case TermIR.Ref("<none>", None) =>
                call.argsList match
                  case Some(args) if args.size == expectedArgs + 1 =>
                    val recvTerm = args.head
                    val argTerms = args.tail
                    for
                      recvEval <- ctx.compileTerm(recvTerm)
                      argEvals <- util.Args.compileArgsList(argTerms, compileArg)
                    yield f(recvEval, argEvals)
                  case _ =>
                    ComptimeDebug.log(s"[comptime] arity mismatch for $name: ${call.args.map(_.size)}")
                    Left(ComptimeError.UnsupportedArity(name, ""))
              case _ =>
                ComptimeDebug.log(s"[comptime] arity mismatch for $name: ${call.args.map(_.size)}")
                Left(ComptimeError.UnsupportedArity(name, ""))

  private def normalizedArgs: List[List[TermIR]] =
    call.args.filter(_.nonEmpty)

  def compileArgsFirst(ctx: RuleContext, name: String): Either[ComptimeError, List[Eval]] =
    call.args match
      case args :: _ => util.Args.compileArgsList(args, ctx.compileTerm)
      case Nil =>
        ComptimeDebug.log(s"[comptime] arity mismatch for $name: ${call.args.map(_.size)}")
        Left(ComptimeError.UnsupportedArity(name, ""))

  def compileArgsAll(ctx: RuleContext): Either[ComptimeError, List[Eval]] =
    util.Args.compileArgsList(call.args.flatten, ctx.compileTerm)

  def compileRecv0(
      ctx: RuleContext,
      name: String
  )(
      f: Eval => Eval
  ): Either[ComptimeError, Eval] =
    if args0 then
      for recvEval <- ctx.compileTerm(call.recv)
      yield f(recvEval)
    else
      call.recv match
        case TermIR.Ref("<none>", None) =>
          args1 match
            case Some(recvTerm) =>
              for recvEval <- ctx.compileTerm(recvTerm)
              yield f(recvEval)
            case None =>
              ComptimeDebug.log(s"[comptime] arity mismatch for $name: ${call.args.map(_.size)}")
              Left(ComptimeError.UnsupportedArity(name, ""))
        case _ =>
          ComptimeDebug.log(s"[comptime] arity mismatch for $name: ${call.args.map(_.size)}")
          Left(ComptimeError.UnsupportedArity(name, ""))

  def compileRecv1(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 1, args1.map(List(_)), ctx.compileTerm) { (recvEval, args) =>
      f(recvEval, args.head)
    }

  def compileRecv1Lazy(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 1, args1.map(List(_)), ctx.compileTermLazy) { (recvEval, args) =>
      f(recvEval, args.head)
    }

  def compileRecv2(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 2, args2.map { case (a, b) => List(a, b) }, ctx.compileTerm) { (recvEval, args) =>
      f(recvEval, args.head, args(1))
    }

  def compileRecv3(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval, Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 3, args3.map { case (a, b, c) => List(a, b, c) }, ctx.compileTerm) { (recvEval, args) =>
      f(recvEval, args.head, args(1), args(2))
    }

  def compileRecv4(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval, Eval, Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 4, args4.map { case (a, b, c, d) => List(a, b, c, d) }, ctx.compileTerm) { (recvEval, args) =>
      f(recvEval, args.head, args(1), args(2), args(3))
    }

  def compileRecv5(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval, Eval, Eval, Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 5, args5.map { case (a, b, c, d, e) => List(a, b, c, d, e) }, ctx.compileTerm) {
      (recvEval, args) =>
        f(recvEval, args.head, args(1), args(2), args(3), args(4))
    }

  def compileRecv1_1(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 2, args1_1.map { case (a, b) => List(a, b) }, ctx.compileTerm) { (recvEval, args) =>
      f(recvEval, args.head, args(1))
    }

  def compileRecv1_1_1(
      ctx: RuleContext,
      name: String
  )(
      f: (Eval, Eval, Eval, Eval) => Eval
  ): Either[ComptimeError, Eval] =
    compileRecvN(ctx, name, 3, args1_1_1.map { case (a, b, c) => List(a, b, c) }, ctx.compileTerm) { (recvEval, args) =>
      f(recvEval, args.head, args(1), args(2))
    }

  def args0: Boolean =
    normalizedArgs.isEmpty

  def args1: Option[TermIR] =
    normalizedArgs match
      case List(List(a)) => Some(a)
      case _             => None

  def args2: Option[(TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a, b)) => Some((a, b))
      case _                => None

  def args3: Option[(TermIR, TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a, b, c)) => Some((a, b, c))
      case _                   => None

  def args4: Option[(TermIR, TermIR, TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a, b, c, d)) => Some((a, b, c, d))
      case _                      => None

  def args5: Option[(TermIR, TermIR, TermIR, TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a, b, c, d, e)) => Some((a, b, c, d, e))
      case _                         => None

  def args1_1: Option[(TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a), List(b)) => Some((a, b))
      case _                      => None

  def args1_1_1: Option[(TermIR, TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a), List(b), List(c)) => Some((a, b, c))
      case _                               => None

  def args1_2: Option[(TermIR, TermIR, TermIR)] =
    normalizedArgs match
      case List(List(a), List(b, c)) => Some((a, b, c))
      case _                         => None

  def argsList: Option[List[TermIR]] =
    normalizedArgs match
      case List(args) => Some(args)
      case _          => None

// compileArgsList moved to util.Args
