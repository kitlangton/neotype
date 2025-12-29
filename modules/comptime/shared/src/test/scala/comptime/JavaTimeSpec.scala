package comptime

import zio.test.*

import java.time.DayOfWeek
import java.time.Duration
import java.time.LocalDate
import java.time.LocalTime
import java.time.Month

object JavaTimeSpec extends ZIOSpecDefault:
  val spec =
    suite("JavaTimeSpec (comptime)")(
      suite("LocalDate operations")(
        test("parse and accessors") {
          assertTrue(
            comptime(LocalDate.parse("2024-03-15").getYear) == 2024,
            comptime(LocalDate.parse("2024-03-15").getMonthValue) == 3,
            comptime(LocalDate.parse("2024-03-15").getDayOfMonth) == 15
          )
        },
        test("of factory method") {
          assertTrue(
            comptime(LocalDate.of(2024, 3, 15).getYear) == 2024,
            comptime(LocalDate.of(2024, 3, 15).toString) == "2024-03-15"
          )
        },
        test("plus/minus operations") {
          assertTrue(
            comptime(LocalDate.of(2024, 3, 15).plusDays(5).getDayOfMonth) == 20,
            comptime(LocalDate.of(2024, 3, 15).minusMonths(1).getMonthValue) == 2
          )
        },
        test("comparisons") {
          assertTrue(
            comptime(LocalDate.of(2024, 1, 1).isBefore(LocalDate.of(2024, 12, 31))) == true,
            comptime(LocalDate.of(2024, 12, 31).isAfter(LocalDate.of(2024, 1, 1))) == true
          )
        }
      ),
      suite("LocalTime operations")(
        test("parse and accessors") {
          assertTrue(
            comptime(LocalTime.parse("14:30:00").getHour) == 14,
            comptime(LocalTime.parse("14:30:00").getMinute) == 30
          )
        },
        test("of factory method") {
          assertTrue(
            comptime(LocalTime.of(14, 30).getHour) == 14,
            comptime(LocalTime.of(14, 30, 45).getSecond) == 45
          )
        }
      ),
      suite("Duration operations")(
        test("of factory methods") {
          assertTrue(
            comptime(Duration.ofHours(2).toMinutes) == 120,
            comptime(Duration.ofMinutes(90).toHours) == 1,
            comptime(Duration.ofSeconds(3661).toHours) == 1
          )
        },
        test("arithmetic") {
          assertTrue(
            comptime(Duration.ofHours(1).plus(Duration.ofMinutes(30)).toMinutes) == 90,
            comptime(Duration.ofHours(2).minus(Duration.ofMinutes(30)).toMinutes) == 90
          )
        },
        test("ZERO constant") {
          assertTrue(
            comptime(Duration.ZERO.isZero) == true
          )
        }
      ),
      suite("Month and DayOfWeek enums")(
        test("Month enum access") {
          assertTrue(
            comptime(Month.MARCH.getValue) == 3,
            comptime(Month.DECEMBER.getValue) == 12
          )
        },
        test("DayOfWeek enum access") {
          assertTrue(
            comptime(DayOfWeek.FRIDAY.getValue) == 5,
            comptime(DayOfWeek.MONDAY.getValue) == 1
          )
        },
        test("LocalDate getMonth returns Month enum") {
          assertTrue(
            comptime(LocalDate.of(2024, 3, 15).getMonth) == Month.MARCH
          )
        },
        test("LocalDate getDayOfWeek returns DayOfWeek enum") {
          // March 15, 2024 is a Friday
          assertTrue(
            comptime(LocalDate.of(2024, 3, 15).getDayOfWeek) == DayOfWeek.FRIDAY
          )
        }
      )
    )
