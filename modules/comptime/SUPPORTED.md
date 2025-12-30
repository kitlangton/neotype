# Comptime Support (Overview)

This is a concise, test-backed summary of what `comptime(...)` evaluates today. The authoritative source of truth is the test suite under
`modules/comptime/shared/src/test/scala/comptime` (see `ComptimeSpec`, `ExtraExpressionsSpec`, `IterableSpec`, `SeqExtraSpec`, `SeqIndexSpec`, `MapExtraSpec`, `OptionSpec`,
`EitherExtraSpec`, `TryExtraSpec`, `PatternMatchingSpec`, `RegexSpec`, `JavaTimeSpec`, `AdvancedCollectionSpec`, `FillTestSpec`, `ArrayOpsSpec`,
`StringExtraSpec`, `CharOpsSpec`, `NumericAbsSpec`, `NumericExtrasSpec`, `BigNumSpec`, `SetSpec`, `TupleExtraSpec`, `TestEitherOps`, `TryCatchSpec`, `MixedNumericSpec`).

## Language constructs (supported)
- Literals: Int/Long/Float/Double/Boolean/Char/Byte/Short/String/Unit, tuples, collections.
- Union types: `A | B` (e.g., `String | Boolean` for validation patterns like `if cond then "error" else true`).
- Arithmetic and comparisons on numeric primitives.
- Boolean ops including short‑circuit `&&` / `||`.
- `if/else` expressions.
- Blocks with local `val`s.
- `try/catch/finally` expressions (exception type matching, nested handlers, variable binding in catch clauses).
- `throw` expressions (when the thrown value is an existing Throwable, not a constructor call).
- Lambdas used in stdlib ops (e.g., `map`, `filter`, `foldLeft`).
- Basic member/field access when the receiver is a constant (case class fields, product element names).

## Pattern matching (supported)
- Literal patterns, wildcard, variable patterns.
- Typed patterns (`case _: T`, `case x: T`).
- Guards (`if` conditions).
- Alternatives (`case a | b`).
- Tuple patterns (Tuple2–Tuple5).
- Standard extractor patterns:
  - `Option`, `Either`, `Try` (`Some`, `None`, `Left`, `Right`, `Success`, `Failure`).
  - Collections: `List`, `Vector`, `Seq`, `Array`, `Nil`, `::` (cons), `_ *` varargs.
  - Regex extractors (`"...".r`).
- Custom extractors (`unapply` / `unapplySeq`) as exercised by `TestExtractors` and case‑class patterns in tests.
- `@` binders around alternatives and nested patterns (covered by tests).

## Standard library ops (supported)
### Numeric
- `+ - * / % unary_-` for `Int`, `Long`, `Float`, `Double`.
- Comparisons: `== != < <= > >=`.
- Mixed-type operations: Cross-type comparisons (e.g., `Double <= Int`, `Long > Int`) and arithmetic (e.g., `Double + Int`, `Int / Double`) work with automatic widening, matching Scala's behavior.
- `Int`/`Long`/`Double`/`Float`: `abs`, `sign`, `signum`, `max`, `min`.
- `Int`: `toHexString`, `toBinaryString`, `toLong`, `toDouble`, `toFloat`, `toShort`, `toByte`, `toChar`.
- `Int`: bitwise ops `<< >> >>> & | ^`.
- `Long`: bitwise ops `<< >> >>> & | ^`, conversions `toInt`, `toDouble`, `toFloat`.
- `Byte`/`Short`: `abs`, `sign`, `signum`, arithmetic (`+ - * / %`), comparisons.
- Implicit widening conversions: `int2long`, `int2float`, `int2double`, `long2float`, `long2double`, etc.
- `Char` basics: `toUpper`, `toLower`, `asDigit`, `isDigit`, `isLetter`, `isWhitespace`, `isUpper`, `isLower`, `isLetterOrDigit`, `isControl`, `isSpaceChar`.
- `Char` ranges: `to`, `until`.

### Boolean
- `!`, `&`, `|`, `^`, `&&`, `||` (short‑circuit).

### String
- Case/trim: `toUpperCase`, `toLowerCase`, `trim`, `strip`, `stripLeading`, `stripTrailing`, `capitalize`.
- Length/empties: `isBlank`, `isEmpty`, `nonEmpty`, `length`.
- Interpolation: `StringContext.s` (e.g., `s"hello $x"`).
- `head`, `tail`, `last`, `init`, `headOption`, `lastOption`.
- `substring`, `slice`, `split`, `startsWith`, `endsWith`, `contains`, `matches`.
- `indexOf` / `lastIndexOf` (including overloads with `from` index).
- Strip/pad: `stripPrefix`, `stripSuffix`, `stripMargin` (with optional Char), `padTo`.
- Replace: `replace` (Char→Char and String→String), `replaceAll`, `replaceFirst`.
- Comparison: `compareToIgnoreCase`, `equalsIgnoreCase`.
- Conversions: `toInt`, `toLong`, `toDouble`, `toFloat`, `toShort`, `toByte`, `toBoolean`.
- Option conversions: `toIntOption`, `toLongOption`, `toDoubleOption`, `toFloatOption`.
- Collection-like: `reverse`, `filter`, `filterNot`, `map`, `flatMap`, `collect`, `take`, `drop`, `takeRight`, `dropRight`, `mkString`, `forall`.
- Regex construction via `.r`.
- `repeat`, `format` (varargs).

### Regex
- Find ops: `findFirstIn`, `findAllIn`, `findFirstMatchIn`, `findAllMatchIn`.
- Replace ops: `replaceAllIn`, `replaceFirstIn`.
- Split ops: `split`.
- Test ops: `matches`.
- Accessors: `regex`, `pattern`, `toString`.

### Option / Either / Try
- Constructors: `Option(...)`, `Option.empty`, `Some(...)`, `Left(...)`, `Right(...)`, `Try(...)`, `Success(...)`.
- Helpers: `Option.when`, `Option.unless`, `Either.cond`.
- Option combinators: `map`, `flatMap`, `filter`, `filterNot`, `fold`, `getOrElse`, `orElse`, `toRight`, `toLeft`, `flatten`, `orNull`, `collect`, `contains`, `isEmpty`, `nonEmpty`, `isDefined`, `iterator`, `toList`, `toSeq`, `zip`, `sum`, `product`, `withFilter`.
- Either combinators: `map`, `flatMap`, `getOrElse`, `orElse`, `filterOrElse`, `contains`, `forall`, `exists`, `toOption`, `toSeq`, `swap`, `joinLeft`, `joinRight`, `merge`, `left` (for LeftProjection), `left.map`.
- Try combinators: `map`, `flatMap`, `fold`, `getOrElse`, `toOption`, `toEither`, `isSuccess`, `isFailure`, `failed`, `transform`, `recover`, `recoverWith`.

### Collections
- Constructors: `List(...)`, `Vector(...)`, `Set(...)`, `Map(...)`, `Array(...)`, `LazyList(...)`, plus `.empty` variants.
- Factory ops: `List.fill`, `List.tabulate`, `Vector.fill`, `Vector.tabulate`, `Seq.fill`, `Seq.tabulate`.
- Core ops: `size`, `length`, `isEmpty`, `nonEmpty`, `head`, `tail`, `last`, `contains`.
- Seq ops: `take`, `drop`, `takeRight`, `dropRight`, `slice`, `++`, `:+` (appended), `+:` (prepended), `mkString`, `updated`, `reverse`, `distinct`, `distinctBy`, `headOption`, `lastOption`, `indices`, `corresponds`, `sameElements`, `indexOf`, `lastIndexOf` (including overloads with `from`/`end` index), `indexWhere`, `lastIndexWhere` (including overloads with `from`/`end` index), `patch`, `padTo`.
- Predicate ops: `exists`, `forall`, `find`, `count`, `takeWhile`, `dropWhile`, `span`, `partition`.
- Split ops: `splitAt`, `groupBy`, `groupMap`, `groupMapReduce`, `partitionMap`.
- Scan ops: `scanLeft`, `scanRight`, `tapEach`.
- Zip ops: `zip`, `zipWithIndex`, `unzip`, `unzip3`.
- Numeric ops: `sum`, `product` (for Int/Long/Float/Double/BigInt/BigDecimal).
- Ordering ops: `max`, `min`, `maxOption`, `minOption`, `maxBy`, `minBy`, `maxByOption`, `minByOption`, `sorted`, `sortBy`, `sortWith`.
- Partial function ops: `collect`, `collectFirst`.
- Fold ops: `foldLeft`, `foldRight`.
- Reduce ops: `reduce`, `reduceLeft`, `reduceRight`, `reduceOption`, `reduceLeftOption`, `reduceRightOption`.
- Flatten ops: `flatten` (for nested collections), `transpose` (for nested iterables).
- Conversion ops: `toList`, `toVector`, `toSeq`, `toSet`, `toMap`.
- Map ops: `get`, `apply`, `getOrElse` (by‑name), `updated`, `removed`, `++`, `keys`, `values`, `keySet`, `map`, `flatMap`, `filter`, `filterNot`, `transform`.
- Range ops: `to`, `until` (on Int/Char), `Range.apply`, `Range.inclusive`, `by`, `toList`, `toVector`, `toSeq`.
- Iterator ops: `hasNext`, `next`, `toVector`, `toSet`, `toList`, `zip`, `zipAll`, `grouped`, `sliding`, `slice`, `takeWhile`, `dropWhile`, `map`, `flatMap`, `filter`, `filterNot`, `collect`, `Iterator.from(start)`, `Iterator.from(start, step)`, `sum`, `product`, `max`, `min`, `maxOption`, `minOption`, `maxBy`, `minBy`, `maxByOption`, `minByOption`, `mkString`, `reduce`, `reduceOption`, `exists`, `forall`, `find`, `count`.
- LazyList ops: `force`, `LazyList.from(start)`, `LazyList.from(start, step)`, plus standard Seq ops (`head`, `tail`, `isEmpty`, `nonEmpty`, `size`, `take`, `drop`, `takeWhile`, `dropWhile`, `map`, `flatMap`, `filter`, `filterNot`, `foldLeft`, `foldRight`, `reduce`, `toList`, `toVector`, etc.).
- `map`, `flatMap`, `filter`, `filterNot`, `withFilter` (where relevant). Note: `withFilter` is treated as `filter` for comptime purposes (eager evaluation).
- For-comprehensions with guards: `for (x <- xs if pred) yield f(x)` works for both collections and Option.
- Array ops (via Seq conversion): `toList`, `toSeq`, `size`, `length`, `head`, `tail`, `take`, `drop`, `filter`, `exists`, `forall`, `foldLeft`, `foldRight`. Note: `Array.map`/`flatMap` require ClassTag and are not supported.

### Java stdlib
- `java.net.URI`, `java.net.URL`, `java.util.UUID` construction/validation where used in tests.

### java.time
- `LocalDate`: constructors (`of`, `parse`), accessors (`getYear`, `getMonth`, `getMonthValue`, `getDayOfMonth`, `getDayOfWeek`, `getDayOfYear`, `lengthOfMonth`, `lengthOfYear`, `isLeapYear`), arithmetic (`plusDays`, `plusWeeks`, `plusMonths`, `plusYears`, `minusDays`, `minusWeeks`, `minusMonths`, `minusYears`), with-methods (`withYear`, `withMonth`, `withDayOfMonth`, `withDayOfYear`), comparisons (`isBefore`, `isAfter`, `isEqual`), `toString`.
- `LocalTime`: constructors (`of`, `parse`), accessors (`getHour`, `getMinute`, `getSecond`, `getNano`, `toSecondOfDay`, `toNanoOfDay`), arithmetic (`plusHours`, `plusMinutes`, `plusSeconds`, `plusNanos`, `minusHours`, `minusMinutes`, `minusSeconds`, `minusNanos`), with-methods (`withHour`, `withMinute`, `withSecond`, `withNano`), comparisons (`isBefore`, `isAfter`), `toString`.
- `Duration`: factory methods (`ofDays`, `ofHours`, `ofMinutes`, `ofSeconds`, `ofMillis`, `ofNanos`, `parse`, `ZERO`), conversions (`toDays`, `toHours`, `toMinutes`, `toSeconds`, `toMillis`, `toNanos`), part accessors (`toDaysPart`, `toHoursPart`, `toMinutesPart`, `toSecondsPart`, `toMillisPart`, `toNanosPart`, `getSeconds`, `getNano`), arithmetic (`plus`, `minus`, `multipliedBy`, `dividedBy`, `plusDays`, `plusHours`, `plusMinutes`, `plusSeconds`, `plusMillis`, `plusNanos`, `minusDays`, `minusHours`, `minusMinutes`, `minusSeconds`, `minusMillis`, `minusNanos`), other (`abs`, `negated`, `isZero`, `isNegative`, `compareTo`), `toString`.
- `Month`: `getValue` (enum constant access works: `Month.MARCH`, etc.).
- `DayOfWeek`: `getValue` (enum constant access works: `DayOfWeek.FRIDAY`, etc.).

## Not guaranteed / likely gaps
These are **not guaranteed** unless tests cover them. If you need any of these, add tests first:
- `while` loops, `lazy val`, `var` (mutable state not supported).
- Exception constructors (`new RuntimeException("msg")`) - use naturally-occurring exceptions instead.
- Arbitrary method calls outside the whitelist of stdlib rules.
- Exotic `unapplySeq` patterns returning non‑Seq/Array, or custom extractors beyond the tested shapes.
- Deeply nested alternative patterns with bindings in all branches (some are tested, not all).
- General reflection or IO at compile time (explicitly out of scope).
- `Array.fill` / `Array.tabulate` (requires `ClassTag` implicit support, not yet implemented).
- `ArrayOps.map` / `ArrayOps.flatMap` (require ClassTag for return type).

## Where to look
- Specs (exhaustive): `modules/comptime/shared/src/test/scala/comptime/*Spec.scala`
- Rules: `modules/comptime/shared/src/main/scala/comptime/rules/RuleHelpersCore.scala` and
  `modules/comptime/shared/src/main/scala/comptime/rules/stdlib/**`
