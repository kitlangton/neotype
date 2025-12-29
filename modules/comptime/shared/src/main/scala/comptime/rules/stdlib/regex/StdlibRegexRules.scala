// Stdlib rules for scala.util.matching.Regex (hand-maintained).
package comptime

import scala.util.matching.Regex

import RuleHelpers.*

private[comptime] object StdlibRegexTables:
  type R = Regex

  // Zero-arg ops on Regex
  val nullary: List[(String, R => Any)] = List(
    "regex"    -> (_.regex),
    "pattern"  -> (_.pattern),
    "toString" -> (_.toString)
  )

  // Regex operations that take a single String argument
  val ops1String: List[(String, (R, String) => Any)] = List(
    "findFirstIn"      -> ((r, s) => r.findFirstIn(s)),
    "findAllIn"        -> ((r, s) => r.findAllIn(s).iterator),
    "findFirstMatchIn" -> ((r, s) => r.findFirstMatchIn(s)),
    "findAllMatchIn"   -> ((r, s) => r.findAllMatchIn(s).iterator),
    "split"            -> ((r, s) => r.split(s)),
    "matches"          -> ((r, s) => r.matches(s))
  )

  // Regex operations that take two String arguments (source, replacement)
  val ops2StringString: List[(String, (R, String, String) => Any)] = List(
    "replaceAllIn"   -> ((r, source, replacement) => r.replaceAllIn(source, replacement)),
    "replaceFirstIn" -> ((r, source, replacement) => r.replaceFirstIn(source, replacement))
  )

private[comptime] object StdlibRegexBasicRules:
  def nullaryOps(rule: RulesFor[Regex]): List[CallRule] =
    rule.opsList(StdlibRegexTables.nullary)

  def ops1String(rule: RulesFor[Regex]): List[CallRule] =
    rule.ops1List[String](StdlibRegexTables.ops1String)

  def ops2StringString(rule: RulesFor[Regex]): List[CallRule] =
    rule.ops2List[String, String](StdlibRegexTables.ops2StringString)

object StdlibRegexRules:
  // Regex receiver - matches scala.util.matching.Regex
  private val regexRecv = Recv.union("scala.util.matching.Regex")
  private val regexRule = RulesFor[Regex](regexRecv)

  private val regexNullary: List[CallRule] =
    StdlibRegexBasicRules.nullaryOps(regexRule)

  private val regexOps1: List[CallRule] =
    StdlibRegexBasicRules.ops1String(regexRule)

  private val regexOps2: List[CallRule] =
    StdlibRegexBasicRules.ops2StringString(regexRule)

  val rules: List[CallRule] =
    RuleHelpers.concat(regexNullary, regexOps1, regexOps2)
