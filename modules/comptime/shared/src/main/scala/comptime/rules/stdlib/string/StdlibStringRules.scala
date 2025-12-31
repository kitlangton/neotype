// Stdlib rules (hand-maintained).
package comptime

import RuleHelpers.*
import StdlibCollectionCommon.takeDropOps

private[comptime] object StdlibStringTables:
  val heads: List[(String, String => Any)] = List(
    "head"       -> (_.head),
    "last"       -> (_.last),
    "tail"       -> (_.tail),
    "init"       -> (_.init),
    "headOption" -> (_.headOption),
    "lastOption" -> (_.lastOption)
  )

  val caseAndTrim: List[(String, String => Any)] = List(
    "toUpperCase"   -> (_.toUpperCase),
    "toLowerCase"   -> (_.toLowerCase),
    "trim"          -> (_.trim),
    "strip"         -> (_.strip),
    "stripLeading"  -> (_.stripLeading),
    "stripTrailing" -> (_.stripTrailing)
  )

  val empties: List[(String, String => Any)] = List(
    "isBlank"  -> (_.isBlank),
    "isEmpty"  -> (_.isEmpty),
    "nonEmpty" -> (_.nonEmpty),
    "length"   -> (_.length)
  )

  val regex: List[(String, String => Any)] = List(
    "r" -> (s => new scala.util.matching.Regex(s))
  )

  val listOps: List[(String, String => Any)] = List(
    "toList" -> (_.toList)
  )

  val getBytesOps: List[(String, String => Any)] = List(
    "getBytes" -> (_.getBytes)
  )

  val getBytesCharset: List[(String, (String, String) => Any)] = List(
    "getBytes" -> ((s, charset) => s.getBytes(charset))
  )

  // StringOps conversion methods
  val conversionOps: List[(String, String => Any)] = List(
    "toInt"          -> (_.toInt),
    "toLong"         -> (_.toLong),
    "toDouble"       -> (_.toDouble),
    "toFloat"        -> (_.toFloat),
    "toShort"        -> (_.toShort),
    "toByte"         -> (_.toByte),
    "toBoolean"      -> (_.toBoolean),
    "toIntOption"    -> (_.toIntOption),
    "toLongOption"   -> (_.toLongOption),
    "toDoubleOption" -> (_.toDoubleOption),
    "toFloatOption"  -> (_.toFloatOption)
  )

  // StringOps collection-like methods
  val stringCollectionOps: List[(String, String => Any)] = List(
    "reverse"     -> (_.reverse),
    "capitalize"  -> (_.capitalize),
    "stripMargin" -> (_.stripMargin)
  )

  // StringOps operations with Char argument
  val stringOps1Char: List[(String, (String, Char) => Any)] = List(
    "stripMargin" -> ((s, c) => s.stripMargin(c))
  )

  // StringOps operations with Int, Char arguments
  val stringOps2IntChar: List[(String, (String, Int, Char) => Any)] = List(
    "padTo" -> ((s, len, c) => s.padTo(len, c))
  )

  val stringCollectionOps1Pred: List[(String, (String, Char => Boolean) => Any)] = List(
    "filter"    -> ((s, f) => s.filter(f)),
    "filterNot" -> ((s, f) => s.filterNot(f))
  )

  // String.map and flatMap are handled separately to use type args from AST

  val stringCollectionOps1Int: List[(String, (String, Int) => Any)] = List(
    "takeRight" -> ((s, n) => s.takeRight(n)),
    "dropRight" -> ((s, n) => s.dropRight(n))
  )

  val stringMkString1: List[(String, (String, String) => Any)] = List(
    "mkString" -> ((s, sep) => s.mkString(sep))
  )

  val stringMkString3: List[(String, (String, String, String, String) => Any)] = List(
    "mkString" -> ((s, start, sep, end) => s.mkString(start, sep, end))
  )

  val ops1Any: List[(String, (String, Any) => Any)] = List(
    "+"           -> ((s, a) => s + String.valueOf(a)),
    "contains"    -> ((s, a) => s.contains(String.valueOf(a))),
    "matches"     -> ((s, a) => s.matches(String.valueOf(a))),
    "startsWith"  -> ((s, a) => s.startsWith(String.valueOf(a))),
    "endsWith"    -> ((s, a) => s.endsWith(String.valueOf(a))),
    "stripPrefix" -> ((s, a) => s.stripPrefix(String.valueOf(a))),
    "stripSuffix" -> ((s, a) => s.stripSuffix(String.valueOf(a)))
  )

  val ops1String: List[(String, (String, String) => Any)] = List(
    "split"               -> ((s, sep) => s.split(sep)),
    "compareToIgnoreCase" -> ((s, other) => s.compareToIgnoreCase(other)),
    "equalsIgnoreCase"    -> ((s, other) => s.equalsIgnoreCase(other))
  )

  val ops1CharPred: List[(String, (String, Char => Boolean) => Any)] = List(
    "forall" -> ((s, f) => s.forall(f))
  )

  val ops2AnyInt: List[(String, (String, Any, Int) => Any)] = List(
    "startsWith" -> ((s, a, i) => s.startsWith(String.valueOf(a), i))
  )

  val ops2StringInt: List[(String, (String, String, Int) => Any)] = List(
    "split" -> ((s, sep, limit) => s.split(sep, limit))
  )

  val ops2IntInt: List[(String, (String, Int, Int) => Any)] = List(
    "slice"     -> ((s, from, until) => s.slice(from, until)),
    "substring" -> ((s, from, until) => s.substring(from, until))
  )

  val ops2StringString: List[(String, (String, String, String) => Any)] = List(
    "replaceAll"   -> ((s, regex, replacement) => s.replaceAll(regex, replacement)),
    "replaceFirst" -> ((s, regex, replacement) => s.replaceFirst(regex, replacement))
  )

  val substring1: List[(String, (String, Int) => Any)] = List(
    "substring" -> ((s, from) => s.substring(from)),
    "repeat"    -> ((s, n) => s.repeat(n))
  )

private[comptime] object StdlibStringBasicRules:
  def ops(rule: RulesFor[String]): List[CallRule] =
    RuleHelpers.concat(
      rule.opsList(StdlibStringTables.caseAndTrim),
      rule.opsList(StdlibStringTables.empties),
      rule.opsList(StdlibStringTables.heads),
      rule.opsList(StdlibStringTables.listOps),
      rule.opsList(StdlibStringTables.regex),
      rule.opsList(StdlibStringTables.getBytesOps),
      rule.ops1List[String](StdlibStringTables.getBytesCharset)
    )

  def ops1(rule: RulesFor[String]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops1AnyList(StdlibStringTables.ops1Any),
      rule.ops1List[String](StdlibStringTables.ops1String),
      rule.ops1CharPredList(StdlibStringTables.ops1CharPred)
    )

  def ops2(rule: RulesFor[String]): List[CallRule] =
    RuleHelpers.concat(
      rule.ops2AnyIntList(StdlibStringTables.ops2AnyInt),
      rule.ops2List[String, Int](StdlibStringTables.ops2StringInt),
      rule.ops2IntIntList(StdlibStringTables.ops2IntInt),
      rule.ops2List[String, String](StdlibStringTables.ops2StringString)
    )

  // replace has overloads: replace(Char, Char) and replace(CharSequence, CharSequence)
  // We handle this by using Any args and dispatching at runtime
  def replaceRules(rule: RulesFor[String]): List[CallRule] =
    rule.ops2AnyList(
      List(
        "replace" -> ((s, a, b) =>
          (a, b) match
            case (c1: Char, c2: Char)     => s.replace(c1, c2)
            case (s1: String, s2: String) => s.replace(s1, s2)
            case _                        => s.replace(String.valueOf(a), String.valueOf(b))
        )
      )
    )

  def substring(rule: RulesFor[String]): List[CallRule] =
    rule.ops1IntList(StdlibStringTables.substring1)

  def takeDrop(rule: RulesFor[String]): List[CallRule] =
    takeDropOps(rule)((s, n) => s.take(n), (s, n) => s.drop(n))

  def applyCharAt(rule: RulesFor[String]): List[CallRule] =
    rule.rules[Int, Char]("apply", "charAt")((s, i) => s.charAt(i))

private[comptime] object StdlibStringHelpers:
  def indexRules(rule: RulesFor[String]): List[CallRule] =
    def evalIndex(name: String, from: Option[Int])(s: String, arg: Any): Int =
      val useIndexOf = name == "indexOf"
      def evalInt(i: Int): Int =
        from match
          case Some(pos) => if useIndexOf then s.indexOf(i, pos) else s.lastIndexOf(i, pos)
          case None      => if useIndexOf then s.indexOf(i) else s.lastIndexOf(i)
      def evalChar(c: Char): Int =
        from match
          case Some(pos) => if useIndexOf then s.indexOf(c, pos) else s.lastIndexOf(c, pos)
          case None      => if useIndexOf then s.indexOf(c) else s.lastIndexOf(c)
      def evalString(str: String): Int =
        from match
          case Some(pos) => if useIndexOf then s.indexOf(str, pos) else s.lastIndexOf(str, pos)
          case None      => if useIndexOf then s.indexOf(str) else s.lastIndexOf(str)
      arg match
        case i: Int      => evalInt(i)
        case c: Char     => evalChar(c)
        case str: String => evalString(str)
        case other       => evalString(other.toString)

    RuleHelpers.concat(
      rule.ops1ByNameAny("indexOf", "lastIndexOf")(name => (s, a) => evalIndex(name, None)(s, a)),
      rule.ops2ByName[Any, Int]("indexOf", "lastIndexOf")(name => (s, a, from) => evalIndex(name, Some(from))(s, a))
    )

private[comptime] object StdlibStringContextRules:
  // StringContext.s interpolation: s"Hello $name" => StringContext("Hello ", "").s(name)
  private val stringContextRecv      = Recv.union("scala.StringContext")
  private val stringContextCompanion = Recv.module("scala.StringContext")

  def rules: List[CallRule] =
    List(
      // StringContext.apply(parts*) - constructs a StringContext from parts
      RuleHelpersVarargs.ruleVarargsRecv(stringContextCompanion, "apply") { args =>
        StringContext(args.map(_.asInstanceOf[String])*)
      },
      // StringContext.s(args*) - interleaves parts with stringified args
      RuleHelpersVarargs.ruleVarargsRecvWithReceiver(stringContextRecv, "s") { (ctx, args) =>
        val sc    = ctx.asInstanceOf[StringContext]
        val parts = sc.parts.iterator
        val sb    = new StringBuilder(parts.next())
        args.foreach { arg =>
          sb.append(String.valueOf(arg))
          if parts.hasNext then sb.append(parts.next())
        }
        sb.toString
      }
    )

// String.map and flatMap rules that use call.targs to determine return type.
// Both have two overloads - one returning String (no type param), one returning IndexedSeq[B] (has type param).
// We check call.targs.isEmpty to determine which overload was chosen by Scala.
private[comptime] object StdlibStringMapFlatMapRules:
  private val stringOpsRecv = Recv.union("scala.collection.StringOps")

  // StringOps.map:
  //   def map(f: Char => Char): String          - no type param, targs is empty
  //   def map[B](f: Char => B): IndexedSeq[B]   - has type param, targs is non-empty
  val mapRule: CallRule =
    RuleDsl
      .rule("map")
      .recv(stringOpsRecv)
      .a1
      .compile("StringOps.map") { (call, ctx) =>
        for
          recvEval <- ctx.compileTerm(call.recv)
          argEval  <- ctx.compileTerm(call.args.head.head)
        yield
          val returnsString = call.targs.isEmpty
          Eval.Apply2(
            recvEval,
            argEval,
            Eval.Value(returnsString),
            (recv: Any, fn: Any, isStringResult: Any) =>
              val s = recv.asInstanceOf[String]
              val f = fn.asInstanceOf[Char => Any]
              if isStringResult.asInstanceOf[Boolean] then s.map(c => f(c).asInstanceOf[Char])
              else s.map(f)
          )
      }

  // StringOps.flatMap:
  //   def flatMap(f: Char => String): String                    - no type param, targs is empty
  //   def flatMap[B](f: Char => IterableOnce[B]): IndexedSeq[B] - has type param, targs is non-empty
  val flatMapRule: CallRule =
    RuleDsl
      .rule("flatMap")
      .recv(stringOpsRecv)
      .a1
      .compile("StringOps.flatMap") { (call, ctx) =>
        for
          recvEval <- ctx.compileTerm(call.recv)
          argEval  <- ctx.compileTerm(call.args.head.head)
        yield
          val returnsString = call.targs.isEmpty
          Eval.Apply2(
            recvEval,
            argEval,
            Eval.Value(returnsString),
            (recv: Any, fn: Any, isStringResult: Any) =>
              val s = recv.asInstanceOf[String]
              val f = fn.asInstanceOf[Char => Any]
              if isStringResult.asInstanceOf[Boolean] then
                // No type param means Char => String overload, returns String
                s.flatMap(c => f(c).asInstanceOf[String])
              else
                // Has type param [B], returns IndexedSeq[B]
                s.flatMap(c => f(c).asInstanceOf[IterableOnce[Any]])
          )
      }

  // StringOps.collect:
  //   def collect(pf: PartialFunction[Char, Char]): String       - no type param, targs is empty
  //   def collect[B](pf: PartialFunction[Char, B]): IndexedSeq[B] - has type param, targs is non-empty
  val collectRule: CallRule =
    RuleDsl
      .rule("collect")
      .recv(stringOpsRecv)
      .a1
      .compile("StringOps.collect") { (call, ctx) =>
        for
          recvEval <- ctx.compileTerm(call.recv)
          argEval  <- ctx.compileTerm(call.args.head.head)
        yield
          val returnsString = call.targs.isEmpty
          Eval.Apply2(
            recvEval,
            argEval,
            Eval.Value(returnsString),
            (recv: Any, fn: Any, isStringResult: Any) =>
              val s  = recv.asInstanceOf[String]
              val pf = fn.asInstanceOf[PartialFunction[Char, Any]]
              if isStringResult.asInstanceOf[Boolean] then
                // No type param means PartialFunction[Char, Char], returns String
                s.collect(pf.asInstanceOf[PartialFunction[Char, Char]])
              else
                // Has type param [B], returns IndexedSeq[B]
                s.collect(pf)
          )
      }

  val rules: List[CallRule] = List(mapRule, flatMapRule, collectRule)

private[comptime] object StdlibStringRules:
  private val stringRule = RulesFor.string
  // StringOps is the implicit wrapper that provides toInt, toLong, etc.
  private val stringOpsRecv = Recv.union("scala.collection.StringOps")
  private val stringOpsRule = RulesFor[String](stringOpsRecv)

  private val stringBasics: List[CallRule] =
    RuleHelpers.concat(
      StdlibStringBasicRules.ops(stringRule),
      StdlibStringBasicRules.ops1(stringRule),
      StdlibStringBasicRules.ops2(stringRule),
      StdlibStringBasicRules.replaceRules(stringRule),
      StdlibStringBasicRules.substring(stringRule),
      StdlibStringBasicRules.applyCharAt(stringRule),
      StdlibStringBasicRules.takeDrop(stringRule)
    )

  private val stringIndexes: List[CallRule] =
    StdlibStringHelpers.indexRules(stringRule)

  // StringOps methods (toInt, toLong, etc.)
  private val stringOpsConversions: List[CallRule] =
    stringOpsRule.opsList(StdlibStringTables.conversionOps)

  // StringOps collection-like methods
  private val stringOpsCollection: List[CallRule] =
    RuleHelpers.concat(
      stringOpsRule.opsList(StdlibStringTables.stringCollectionOps),
      stringOpsRule.ops1CharPredList(StdlibStringTables.stringCollectionOps1Pred),
      stringOpsRule.ops1IntList(StdlibStringTables.stringCollectionOps1Int),
      stringOpsRule.ops1List[String](StdlibStringTables.stringMkString1),
      stringOpsRule.ops3List[String, String, String](StdlibStringTables.stringMkString3),
      stringOpsRule.ops1List[Char](StdlibStringTables.stringOps1Char),
      stringOpsRule.ops2List[Int, Char](StdlibStringTables.stringOps2IntChar)
    )

  // StringOps format method (varargs)
  private val stringOpsFormat: List[CallRule] =
    List(
      RuleHelpersVarargs.ruleVarargsRecvWithReceiver(stringOpsRecv, "format") { (s, args) =>
        s.asInstanceOf[String].format(args*)
      }
    )

  def rules: List[CallRule] =
    RuleHelpers.concat(
      stringBasics,
      stringIndexes,
      stringOpsConversions,
      stringOpsCollection,
      stringOpsFormat,
      StdlibStringContextRules.rules,
      StdlibStringMapFlatMapRules.rules
    )
