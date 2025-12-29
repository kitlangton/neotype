// Stdlib rules (hand-maintained).
package comptime

object StdlibRules:
  val rules: List[RuleSpec] =
    RuleHelpers.concat(
      StdlibPredefRules.rules,
      StdlibStringRules.rules,
      StdlibRegexRules.rules,
      StdlibNumericRules.rules,
      StdlibMathRules.rules,
      StdlibOptionRules.rules,
      StdlibEitherRules.rules,
      StdlibTryRules.rules,
      StdlibJavaRules.rules,
      StdlibTimeRules.rules,
      StdlibCollectionRules.rules
    )
