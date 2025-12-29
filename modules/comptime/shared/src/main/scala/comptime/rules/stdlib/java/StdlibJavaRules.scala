// Stdlib rules (hand-maintained).
package comptime

import RuleHelpers.*

private[comptime] object StdlibJavaCtorRules:
  def uri(recv: RecvPred): List[CallRule] =
    ruleRecv1[Any, String, java.net.URI](recv, "<init>")((_, value) => new java.net.URI(value)) :: Nil

  def uuid(recv: RecvPred): List[CallRule] =
    RuleHelpers.concat(
      RulesFor
        .any(recv)
        .arg1s[String](
          "fromString" -> java.util.UUID.fromString
        ),
      RulesFor[java.util.UUID](recv).rules("toString")(_.toString)
    )

object StdlibJavaRules:
  private val uriRecv  = Recv.union("java.net.URI")
  private val uuidRecv = Recv.union("java.util.UUID")

  private val uriCtorRules: List[CallRule] =
    StdlibJavaCtorRules.uri(uriRecv)

  private val uuidCtorRules: List[CallRule] =
    StdlibJavaCtorRules.uuid(uuidRecv)

  val rules: List[CallRule] =
    RuleHelpers.concat(uriCtorRules, uuidCtorRules)
