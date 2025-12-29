package comptime

import scala.quoted.*

object MacroExprsCaseClass:
  def summonExprOpt[A: Type](using Quotes): Option[ToExpr[A]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val sym = tpe.typeSymbol

    // Only handle case classes
    if !sym.flags.is(Flags.Case) || sym.flags.is(Flags.Trait) then return None

    // Get primary constructor and its parameter lists
    val ctor       = sym.primaryConstructor
    val paramLists = ctor.paramSymss.filter(_.forall(_.isTerm))

    if paramLists.isEmpty || paramLists.forall(_.isEmpty) then return None

    // Build field info: (name, type, ToExpr) for each parameter
    case class FieldInfo(name: String, tpe: TypeRepr, toExpr: ToExpr[Any])

    val fieldInfosOpt: Option[List[List[FieldInfo]]] =
      paramLists.foldLeft(Option(List.empty[List[FieldInfo]])) {
        case (None, _) => None
        case (Some(acc), params) =>
          val paramInfos = params.foldLeft(Option(List.empty[FieldInfo])) {
            case (None, _) => None
            case (Some(infos), param) =>
              val fieldTpe = tpe.memberType(param)
              fieldTpe.asType match
                case '[t] =>
                  MacroExprs.summonExprOpt[t] match
                    case Some(te) => Some(infos :+ FieldInfo(param.name, fieldTpe, te.asInstanceOf[ToExpr[Any]]))
                    case None     => None
          }
          paramInfos.map(acc :+ _)
      }

    fieldInfosOpt.map { fieldLists =>
      new ToExpr[A]:
        def apply(value: A)(using Quotes): Expr[A] =
          import quotes.reflect.*

          // Build a map of field name -> value from CaseClassValue or Product
          // All case classes extend Product, so no reflection fallback needed
          val fieldMap: Map[String, Any] = value match
            case cc: CaseClassValue =>
              cc.fields.toMap
            case prod: Product =>
              // Use caseFields to get names in order, then zip with product elements
              val sym   = TypeRepr.of[A].typeSymbol
              val names = sym.caseFields.map(_.name)
              names.zip((0 until prod.productArity).map(prod.productElement)).toMap

          // Build argument lists for each parameter list
          val argLists: List[List[Term]] = fieldLists.map { fields =>
            fields.map { fi =>
              val fieldValue = fieldMap.getOrElse(
                fi.name,
                sys.error(s"[comptime bug] Missing field '${fi.name}' in ${value.getClass.getName}")
              )
              fi.tpe.asType match
                case '[t] =>
                  given ToExpr[t] = fi.toExpr.asInstanceOf[ToExpr[t]]
                  Expr(fieldValue.asInstanceOf[t]).asTerm
            }
          }

          // Use constructor directly (safer than companion.apply)
          val tpe        = TypeRepr.of[A]
          val ctor       = tpe.typeSymbol.primaryConstructor
          val newExpr    = New(TypeTree.of[A])
          val ctorSelect = Select(newExpr, ctor)

          // Apply each argument list
          val result = argLists.foldLeft[Term](ctorSelect) { (term, args) =>
            Apply(term, args)
          }

          result.asExprOf[A]
    }
