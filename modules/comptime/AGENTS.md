# Comptime Agent Notes

## What this module is
`comptime` is Neotype’s compile‑time evaluator — a very limited analog to Zig’s `comptime`. It takes a Scala expression (via macros), lowers
it to a small IR, and evaluates it with a rule engine so that validation and construction can be done at compile time. The intent is to
pre‑optimize certain expressions beyond what `inline` alone can do, while keeping the surface area deliberately small and well‑tested.
The behavior is driven by a whitelist of supported language constructs and stdlib rules (see `modules/comptime/SUPPORTED.md` and the tests
under `modules/comptime/shared/src/test/scala/comptime`).

## Principles
- Prefer concise, table‑driven rules (typed pairs) and `RulesFor` helpers.
- Avoid code generation unless it clearly pays off; keep rules as plain Scala.
- Preserve rule ordering semantics: the first matching rule wins.
- Performance changes should be measured (see `modules/comptime/PERF_PLAN.md`).

## Rule authoring
- Prefer `RulesFor` helpers (`ops*`, `byName_*`, `rules`) at call sites.
- When using typed rule tables, use `opsList` / `ops1List` / `ops2List` / `ops3List` and `byName_*List` helpers to avoid `: _*`.
- Introduce a helper before repeating `compileRecv*` or manual `Eval.Apply*` blocks.
- Keep shared tables local unless reused across multiple rule groups.

## Naming conventions

### S/L Pattern for lazy (by-name) arguments
Methods that handle by-name arguments use an S/L suffix where:
- `S` = Strict (evaluated immediately)
- `L` = Lazy (by-name, evaluated on demand)

The pattern describes argument positions left-to-right:
```
byName_L      # 1 arg, lazy
byName_SL     # 2 args: strict, then lazy (e.g., getOrElse)
byName_LS     # 2 args: lazy, then strict (e.g., fold)
byName_SLL    # 3 args: strict, then 2 lazy
```

For curried arities, prefix with the arity pattern:
```
byName1_1_LS  # curried (1)(1), lazy first, strict second
byName1_1_SL  # curried (1)(1), strict first, lazy second
```

### Arity constants
Use underscores to separate curried arg counts:
```
A0, A1, A2, A3     # flat arities
A1_1, A1_2, A2_1   # curried: A1_1 = (1 arg)(1 arg)
```

### Internal helpers
- `compileByName_SL`, `compileByName_LS`, `compileByName_SLL` - compile helpers
- `ruleRecvByName_SL`, `ruleRecvByName_LS` - rule builders

## Performance
- Rule lookup is indexed by name in `CallRuleEngine` (via `RuleEngine`); keep this invariant when editing.
- Only add recv/arity indexing if measurements show a hotspot.

## Debugging
- Use `ComptimeDebug` for rule dispatch logs when diagnosing failures.
- Add minimal, targeted tests rather than broad rewrites.

## Tests
- Main comptime tests live in `modules/comptime/shared/src/test/scala/comptime`.
- Update `modules/comptime/SUPPORTED.md` when feature coverage changes.

## Adding new stdlib rules

### Quick reference for common patterns

| Pattern | Table type | Helper | Use case |
|---------|-----------|--------|----------|
| `recv.op()` | `A => R` | `opsList` | Basic accessors (head, tail, reverse) |
| `recv.op(arg)` | `(A, B) => R` | `ops1List[B]` | Single-arg ops (take, drop) |
| `recv.op(a, b)` | `(A, B, C) => R` | `ops2IntIntList` | Two-arg ops (slice) |
| `recv.op(f)` with `f: Any => Boolean` | `(A, F) => R` | `ops1PredList` | Predicate ops (exists, filter) |
| `recv.op(f)` with `f: Any => Any` | `(A, F) => R` | `ops1FnList` | Transform ops (map, groupBy) |
| `recv.op(f)` with `f: (Any,Any) => Boolean` | custom | `sortWithAny` | Binary predicates (sortWith) |
| `recv.op(f)` with `f: (Any,Any) => Any` | custom | `reduceAny` | Binary ops (reduce, foldLeft) |
| `recv.op()` with implicit | `A => R` | `anyArityOpsList` | Ops with implicits (sum, flatten) |

### Receiver matching

When the call owner differs from expected (e.g., `SeqOps` vs `Seq`), add it to `seqRecv`:
```scala
private val seqRecv = Recv.union(
  "scala.collection.Seq",
  "scala.collection.SeqOps",  // sortWith, sortBy
  "scala.collection.StrictOptimizedIterableOps",  // flatten
  // ...
)
```

### Arity matching for implicits

Methods with implicits (like `sum`, `sorted`, `flatten`) use `anyArityOpsList` because:
- Scala resolves implicits at compile time → arity appears as A0
- But sometimes they show up as A1 or A1_1
- `ASet(Set.empty)` matches any arity

For curried methods with implicits (like `sortBy(f)(implicit ord)`):
- Use `ruleRecv1AnyArity` which tries A1 first, then A1_1
- Or skip and document as unsupported (complex cases)

### Table vs inline rules

Prefer tables for:
- Groups of similar methods (all predicates, all transforms)
- Simple receiver→result patterns

Use inline rules for:
- Methods needing special logic (type dispatch like sum/max)
- By-name arguments (use byName_ helpers)
- Curried methods (use ruleRecv11Or2)
- **Methods with overloads that differ by return type** — need access to `call.targs` (see "Using AST type information" section)

### Runtime typeclass dispatch (TypeClassMaps)

Operations requiring `Ordering` or `Numeric` (like `sum`, `max`, `min`, `minBy`, `maxBy`) use runtime type dispatch via `TypeClassMaps` in `StdlibCollectionHelpers.scala`:

```scala
// Look up Ordering/Numeric based on element's runtime class
xs.max(TypeClassMaps.getOrdering(xs.head, "max"))
xs.sum(TypeClassMaps.getNumeric(xs.head, "sum"))
```

Supported types: Int, Long, Double, Float, Short, Byte, Char, String, BigInt, BigDecimal (plus boxed variants).

To add a new type:
1. Add entry to `TypeClassMaps.orderings` and/or `TypeClassMaps.numerics`
2. Include both primitive and boxed class variants if applicable

## Try/catch implementation notes

### Exception type aliases
Exception types in catch patterns appear as Scala package aliases (e.g., `scala.package$.NumberFormatException`).
These must be mapped to Java classes in `PatternNames.scalaPackageAliases` for pattern matching to work.

When adding new exception types:
1. Add both `scala.package$.ExceptionName` and `scala.ExceptionName` variants
2. Map to the correct Java class (usually `java.lang.ExceptionName`, but `NoSuchElementException` is in `java.util`)

### Exception reconstruction
Exceptions thrown during rule evaluation are caught by `CallRuleEngine` and converted to `Left(EvalException)`.
For try/catch to work, we reconstruct these back to real exceptions via `TermCompiler.exceptionConstructors`.

To add support for catching a new exception type:
1. Add alias to `PatternNames.scalaPackageAliases`
2. Add constructor to `TermCompiler.exceptionConstructors`

### Finally semantics
The implementation in `TermCompiler` follows Scala semantics:
- Finally always runs (even when catch body throws)
- Finally exception replaces any prior result/exception
- Use `withFinally` helper to ensure proper exception propagation

### Testing try/catch
Exception constructors (`new RuntimeException(...)`) are NOT supported.
Tests must use naturally-occurring exceptions:
- `"abc".toInt` → NumberFormatException
- `List(1)(10)` → IndexOutOfBoundsException
- `1 / 0` → ArithmeticException
- `"abc".charAt(10)` → StringIndexOutOfBoundsException

## withFilter / for-comprehension notes

`withFilter` is treated as eager `filter` for comptime purposes. In standard Scala, `withFilter` returns a lazy `WithFilter` wrapper for use in for-comprehensions. Since comptime evaluates eagerly, we just filter immediately.

For-comprehensions with guards work: `for (x <- xs if pred) yield f(x)` is supported for both collections and Option. The `WithFilter.map/flatMap` calls are handled by dedicated rules that delegate to the underlying filtered collection/option.

## Using AST type information (call.targs)

### The problem: type erasure in rules

When defining rules with function arguments, we often erase types to `Any` to handle multiple cases:
```scala
// Erased to handle both Char => Char and Char => Int
val mapRule: (String, Char => Any) => Any = ...
```

This loses type information needed to determine correct behavior. Runtime inspection (checking result types) fails for edge cases like empty collections.

### The solution: use call.targs

The `CallIR` passed to rules contains `targs: List[TypeIR]` — the type arguments from the AST. This preserves the compiler's type information.

**Example: StringOps.map**

StringOps.map has two overloads:
```scala
def map(f: Char => Char): String          // no type param
def map[B](f: Char => B): IndexedSeq[B]   // has type param [B]
```

Scala chooses the overload at compile time:
- `"hello".map(_.toUpper)` → uses Char => Char overload → `targs` is empty
- `"hello".map(_.toInt)` → uses generic overload → `targs` is `List(Ref("scala.Int", ...))`

The rule checks `call.targs.isEmpty` to determine which overload was chosen:
```scala
RuleDsl.rule("map")
  .recv(stringOpsRecv)
  .a1
  .compile("StringOps.map") { (call, ctx) =>
    for
      recvEval <- ctx.compileTerm(call.recv)
      argEval  <- ctx.compileTerm(call.args.head.head)
    yield
      // Empty targs = Char => Char overload, non-empty = generic [B] overload
      val returnsString = call.targs.isEmpty

      Eval.Apply2(recvEval, argEval, Eval.Value(returnsString),
        (recv, fn, isStringResult) =>
          val s = recv.asInstanceOf[String]
          val f = fn.asInstanceOf[Char => Any]
          if isStringResult.asInstanceOf[Boolean] then
            s.map(c => f(c).asInstanceOf[Char])  // String result
          else
            s.map(f)  // IndexedSeq result
      )
  }
```

### When to use call.targs

Use `call.targs` when:
1. **Method has overloads with different return types** based on type parameters
2. **Runtime inspection fails** for edge cases (empty collections, null values)
3. **Behavior depends on compile-time type** rather than runtime value

### How to debug targs

Add debug output to see what the AST contains:
```scala
println(s"[DEBUG] targs=${call.targs}")
```

Common patterns:
- Empty list `List()` → method has no type parameters or uses non-generic overload
- `List(Ref("scala.Int", List()))` → type param is Int
- `List(Ref("scala.Char", List()))` → type param is Char

### TypeIR structure

```scala
sealed trait TypeIR
object TypeIR:
  case class Ref(fullName: String, args: List[TypeIR])  // e.g., Ref("scala.Int", Nil)
  case class AnyType()                                   // unknown/erased type
```

Check type names with pattern matching:
```scala
def isCharType(tpe: TypeIR): Boolean = tpe match
  case TypeIR.Ref(name, _) => name == "scala.Char" || name == "Char"
  case _ => false
```
