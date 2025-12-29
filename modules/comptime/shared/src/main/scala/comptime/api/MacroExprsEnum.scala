package comptime

import scala.quoted.*

object MacroExprsEnum:
  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val sym = tpe.typeSymbol

    // Only handle Scala 3 enums (not Java enums)
    if !sym.flags.is(Flags.Enum) then return None

    // Get the companion module's full name to reconstruct in inner Quotes
    val companion = sym.companionModule
    if companion == Symbol.noSymbol then return None
    val companionFullName = companion.fullName

    Some(
      new ToExpr[A]:
        def apply(value: A)(using Quotes): Expr[A] =
          import quotes.reflect.*
          // Reconstruct the companion symbol from its full name
          val companionSym = Symbol.requiredModule(companionFullName)
          // Use productPrefix instead of toString - toString can be overridden
          // but productPrefix always returns the actual case name
          val caseName     = value.asInstanceOf[Product].productPrefix
          val companionRef = Ref(companionSym)
          val caseSelect   = Select.unique(companionRef, caseName)
          caseSelect.asExprOf[A]
    )
