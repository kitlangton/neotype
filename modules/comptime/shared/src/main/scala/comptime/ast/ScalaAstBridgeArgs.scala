package comptime

import scala.quoted.*

private[comptime] object ScalaAstBridgeArgs:
  def reorderNamedArgs[Q <: Quotes](using
      quotes: Q
  )(args: List[quotes.reflect.Term], paramNames: List[String]): List[quotes.reflect.Term] =
    import quotes.reflect.*
    val hasNamed = args.exists {
      case NamedArg(_, _) => true
      case _              => false
    }
    if !hasNamed || paramNames.isEmpty then args
    else
      val slots = Array.fill[Option[Term]](paramNames.size)(None)
      var pos   = 0
      args.foreach {
        case NamedArg(name, value) =>
          val idx = paramNames.indexOf(name)
          if idx >= 0 then slots(idx) = Some(value)
        case other =>
          while pos < slots.length && slots(pos).isDefined do pos += 1
          if pos < slots.length then
            slots(pos) = Some(other)
            pos += 1
      }
      slots.iterator.collect { case Some(t) => t }.toList

  def mapArgs[Q <: Quotes](using
      quotes: Q
  )(
      args: List[quotes.reflect.Term],
      paramNames: List[String],
      normalize: quotes.reflect.Term => quotes.reflect.Term,
      termToIR: quotes.reflect.Term => TermIR
  ): List[TermIR] =
    import quotes.reflect.*
    val ordered = reorderNamedArgs(args, paramNames)
    ordered.flatMap { arg =>
      normalize(arg) match
        case Repeated(elems, _) => elems.map(termToIR)
        case NamedArg(_, value) => List(termToIR(value))
        case other              => List(termToIR(other))
    }
