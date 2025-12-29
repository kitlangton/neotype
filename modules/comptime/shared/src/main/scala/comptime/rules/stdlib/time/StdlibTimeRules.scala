// Stdlib rules for java.time classes (hand-maintained).
package comptime

import java.time.Duration
import java.time.LocalDate
import java.time.LocalTime
import java.time.Month

import RuleHelpers.*

private[comptime] object StdlibTimeHelpers:
  // Helper to convert Any to Long for duration arithmetic
  def toLong(value: Any): Long =
    value match
      case v: Long  => v
      case v: Int   => v.toLong
      case v: Short => v.toLong
      case v: Byte  => v.toLong
      case other    => throw new RuntimeException(s"Unsupported numeric value for time ops: $other")

  def toInt(value: Any): Int =
    value match
      case v: Int   => v
      case v: Long  => v.toInt
      case v: Short => v.toInt
      case v: Byte  => v.toInt
      case other    => throw new RuntimeException(s"Unsupported numeric value for time ops: $other")

object StdlibTimeRules:
  // === Receiver definitions ===
  private val localDateRecv = Recv.union("java.time.LocalDate")
  private val localTimeRecv = Recv.union("java.time.LocalTime")
  private val durationRecv  = Recv.union("java.time.Duration")

  // === LocalDate rules ===
  private val localDateRule = RulesFor[LocalDate](localDateRecv)

  private val localDateAccessors: List[CallRule] =
    localDateRule.opsList(
      List(
        "getYear"       -> (_.getYear),
        "getMonth"      -> (_.getMonth),
        "getMonthValue" -> (_.getMonthValue),
        "getDayOfMonth" -> (_.getDayOfMonth),
        "getDayOfWeek"  -> (_.getDayOfWeek),
        "getDayOfYear"  -> (_.getDayOfYear),
        "lengthOfMonth" -> (_.lengthOfMonth),
        "lengthOfYear"  -> (_.lengthOfYear),
        "isLeapYear"    -> (_.isLeapYear),
        "toString"      -> (_.toString)
      )
    )

  private val localDatePlusMinusLong: List[CallRule] =
    RulesFor[LocalDate](localDateRecv).ops1AnyList(
      List(
        "plusDays"    -> ((d, n) => d.plusDays(StdlibTimeHelpers.toLong(n))),
        "plusWeeks"   -> ((d, n) => d.plusWeeks(StdlibTimeHelpers.toLong(n))),
        "plusMonths"  -> ((d, n) => d.plusMonths(StdlibTimeHelpers.toLong(n))),
        "plusYears"   -> ((d, n) => d.plusYears(StdlibTimeHelpers.toLong(n))),
        "minusDays"   -> ((d, n) => d.minusDays(StdlibTimeHelpers.toLong(n))),
        "minusWeeks"  -> ((d, n) => d.minusWeeks(StdlibTimeHelpers.toLong(n))),
        "minusMonths" -> ((d, n) => d.minusMonths(StdlibTimeHelpers.toLong(n))),
        "minusYears"  -> ((d, n) => d.minusYears(StdlibTimeHelpers.toLong(n)))
      )
    )

  private val localDateWithOps: List[CallRule] =
    RulesFor[LocalDate](localDateRecv).ops1AnyList(
      List(
        "withYear"       -> ((d, n) => d.withYear(StdlibTimeHelpers.toInt(n))),
        "withMonth"      -> ((d, n) => d.withMonth(StdlibTimeHelpers.toInt(n))),
        "withDayOfMonth" -> ((d, n) => d.withDayOfMonth(StdlibTimeHelpers.toInt(n))),
        "withDayOfYear"  -> ((d, n) => d.withDayOfYear(StdlibTimeHelpers.toInt(n)))
      )
    )

  private val localDateComparisons: List[CallRule] =
    localDateRule.ops1List[LocalDate](
      List(
        "isBefore" -> (_.isBefore(_)),
        "isAfter"  -> (_.isAfter(_)),
        "isEqual"  -> (_.isEqual(_))
      )
    )

  private val localDateStatics: List[CallRule] =
    RuleHelpers.concat(
      ruleStatic1sList[String, LocalDate](
        localDateRecv,
        List(
          "parse" -> LocalDate.parse
        )
      ),
      List(
        ruleRecv3[Any, Int, Int, Int, LocalDate](localDateRecv, "of")((_, year, month, day) =>
          LocalDate.of(year, month, day)
        ),
        ruleRecv3[Any, Int, Month, Int, LocalDate](localDateRecv, "of")((_, year, month, day) =>
          LocalDate.of(year, month, day)
        )
      )
    )

  private val localDateRules: List[CallRule] =
    RuleHelpers.concat(
      localDateAccessors,
      localDatePlusMinusLong,
      localDateWithOps,
      localDateComparisons,
      localDateStatics
    )

  // === LocalTime rules ===
  private val localTimeRule = RulesFor[LocalTime](localTimeRecv)

  private val localTimeAccessors: List[CallRule] =
    localTimeRule.opsList(
      List(
        "getHour"       -> (_.getHour),
        "getMinute"     -> (_.getMinute),
        "getSecond"     -> (_.getSecond),
        "getNano"       -> (_.getNano),
        "toSecondOfDay" -> (_.toSecondOfDay),
        "toNanoOfDay"   -> (_.toNanoOfDay),
        "toString"      -> (_.toString)
      )
    )

  private val localTimePlusMinusLong: List[CallRule] =
    RulesFor[LocalTime](localTimeRecv).ops1AnyList(
      List(
        "plusHours"    -> ((t, n) => t.plusHours(StdlibTimeHelpers.toLong(n))),
        "plusMinutes"  -> ((t, n) => t.plusMinutes(StdlibTimeHelpers.toLong(n))),
        "plusSeconds"  -> ((t, n) => t.plusSeconds(StdlibTimeHelpers.toLong(n))),
        "plusNanos"    -> ((t, n) => t.plusNanos(StdlibTimeHelpers.toLong(n))),
        "minusHours"   -> ((t, n) => t.minusHours(StdlibTimeHelpers.toLong(n))),
        "minusMinutes" -> ((t, n) => t.minusMinutes(StdlibTimeHelpers.toLong(n))),
        "minusSeconds" -> ((t, n) => t.minusSeconds(StdlibTimeHelpers.toLong(n))),
        "minusNanos"   -> ((t, n) => t.minusNanos(StdlibTimeHelpers.toLong(n)))
      )
    )

  private val localTimeWithOps: List[CallRule] =
    RulesFor[LocalTime](localTimeRecv).ops1AnyList(
      List(
        "withHour"   -> ((t, n) => t.withHour(StdlibTimeHelpers.toInt(n))),
        "withMinute" -> ((t, n) => t.withMinute(StdlibTimeHelpers.toInt(n))),
        "withSecond" -> ((t, n) => t.withSecond(StdlibTimeHelpers.toInt(n))),
        "withNano"   -> ((t, n) => t.withNano(StdlibTimeHelpers.toInt(n)))
      )
    )

  private val localTimeComparisons: List[CallRule] =
    localTimeRule.ops1List[LocalTime](
      List(
        "isBefore" -> (_.isBefore(_)),
        "isAfter"  -> (_.isAfter(_))
      )
    )

  private val localTimeStatics: List[CallRule] =
    RuleHelpers.concat(
      ruleStatic1sList[String, LocalTime](
        localTimeRecv,
        List(
          "parse" -> LocalTime.parse
        )
      ),
      List(
        ruleRecv2[Any, Int, Int, LocalTime](localTimeRecv, "of")((_, hour, minute) => LocalTime.of(hour, minute)),
        ruleRecv3[Any, Int, Int, Int, LocalTime](localTimeRecv, "of")((_, hour, minute, second) =>
          LocalTime.of(hour, minute, second)
        ),
        ruleRecv4[Any, Int, Int, Int, Int, LocalTime](localTimeRecv, "of")((_, hour, minute, second, nano) =>
          LocalTime.of(hour, minute, second, nano)
        )
      )
    )

  private val localTimeRules: List[CallRule] =
    RuleHelpers.concat(
      localTimeAccessors,
      localTimePlusMinusLong,
      localTimeWithOps,
      localTimeComparisons,
      localTimeStatics
    )

  // === Duration rules ===
  private val durationRule = RulesFor[Duration](durationRecv)

  private val durationAccessors: List[CallRule] =
    durationRule.opsList(
      List(
        "toDays"        -> (_.toDays),
        "toHours"       -> (_.toHours),
        "toMinutes"     -> (_.toMinutes),
        "toSeconds"     -> (_.toSeconds),
        "toMillis"      -> (_.toMillis),
        "toNanos"       -> (_.toNanos),
        "toDaysPart"    -> (_.toDaysPart),
        "toHoursPart"   -> (_.toHoursPart),
        "toMinutesPart" -> (_.toMinutesPart),
        "toSecondsPart" -> (_.toSecondsPart),
        "toMillisPart"  -> (_.toMillisPart),
        "toNanosPart"   -> (_.toNanosPart),
        "getSeconds"    -> (_.getSeconds),
        "getNano"       -> (_.getNano),
        "abs"           -> (_.abs),
        "negated"       -> (_.negated),
        "isZero"        -> (_.isZero),
        "isNegative"    -> (_.isNegative),
        "toString"      -> (_.toString)
      )
    )

  private val durationArithmetic: List[CallRule] =
    RuleHelpers.concat(
      durationRule.ops1List[Duration](
        List(
          "plus"      -> (_.plus(_)),
          "minus"     -> (_.minus(_)),
          "compareTo" -> (_.compareTo(_))
        )
      ),
      RulesFor[Duration](durationRecv).ops1AnyList(
        List(
          "multipliedBy" -> ((d, n) => d.multipliedBy(StdlibTimeHelpers.toLong(n))),
          "dividedBy"    -> ((d, n) => d.dividedBy(StdlibTimeHelpers.toLong(n))),
          "plusDays"     -> ((d, n) => d.plusDays(StdlibTimeHelpers.toLong(n))),
          "plusHours"    -> ((d, n) => d.plusHours(StdlibTimeHelpers.toLong(n))),
          "plusMinutes"  -> ((d, n) => d.plusMinutes(StdlibTimeHelpers.toLong(n))),
          "plusSeconds"  -> ((d, n) => d.plusSeconds(StdlibTimeHelpers.toLong(n))),
          "plusMillis"   -> ((d, n) => d.plusMillis(StdlibTimeHelpers.toLong(n))),
          "plusNanos"    -> ((d, n) => d.plusNanos(StdlibTimeHelpers.toLong(n))),
          "minusDays"    -> ((d, n) => d.minusDays(StdlibTimeHelpers.toLong(n))),
          "minusHours"   -> ((d, n) => d.minusHours(StdlibTimeHelpers.toLong(n))),
          "minusMinutes" -> ((d, n) => d.minusMinutes(StdlibTimeHelpers.toLong(n))),
          "minusSeconds" -> ((d, n) => d.minusSeconds(StdlibTimeHelpers.toLong(n))),
          "minusMillis"  -> ((d, n) => d.minusMillis(StdlibTimeHelpers.toLong(n))),
          "minusNanos"   -> ((d, n) => d.minusNanos(StdlibTimeHelpers.toLong(n)))
        )
      )
    )

  private val durationStatics: List[CallRule] =
    RuleHelpers.concat(
      ruleStatic1sList[String, Duration](
        durationRecv,
        List(
          "parse" -> Duration.parse
        )
      ),
      RulesFor
        .any(durationRecv)
        .arg1sAnyList(
          List(
            "ofDays"    -> (n => Duration.ofDays(StdlibTimeHelpers.toLong(n))),
            "ofHours"   -> (n => Duration.ofHours(StdlibTimeHelpers.toLong(n))),
            "ofMinutes" -> (n => Duration.ofMinutes(StdlibTimeHelpers.toLong(n))),
            "ofSeconds" -> (n => Duration.ofSeconds(StdlibTimeHelpers.toLong(n))),
            "ofMillis"  -> (n => Duration.ofMillis(StdlibTimeHelpers.toLong(n))),
            "ofNanos"   -> (n => Duration.ofNanos(StdlibTimeHelpers.toLong(n)))
          )
        ),
      List(
        RulesFor.any(durationRecv).constAnyArity("ZERO")(Duration.ZERO)
      )
    )

  private val durationRules: List[CallRule] =
    RuleHelpers.concat(
      durationAccessors,
      durationArithmetic,
      durationStatics
    )

  // === All time rules ===
  val rules: List[CallRule] =
    RuleHelpers.concat(
      localDateRules,
      localTimeRules,
      durationRules
    )
