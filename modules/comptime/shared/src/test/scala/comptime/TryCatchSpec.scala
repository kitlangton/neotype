package comptime

import zio.test.*

object TryCatchSpec extends ZIOSpecDefault:
  val spec =
    suite("try/catch/finally")(
      suite("basic try/catch")(
        test("try without exception returns body result") {
          assertTrue(
            comptime(
              try 42
              catch case _: Exception => -1
            ) == 42
          )
        },
        test("catch handles exception from string conversion") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch case _: NumberFormatException => 99
            ) == 99
          )
        },
        test("catch with variable binding") {
          assertTrue(
            comptime(try
              "bad".toInt; false
            catch case e: NumberFormatException => e.getMessage.contains("bad")) == true
          )
        },
        test("arithmetic in try body") {
          assertTrue(
            comptime(
              try 1 + 2 + 3
              catch case _: Exception => 0
            ) == 6
          )
        },
        test("string operations in try") {
          assertTrue(
            comptime(
              try "hello".toUpperCase
              catch case _: Exception => "error"
            ) == "HELLO"
          )
        }
      ),
      suite("finally")(
        test("finally runs on success") {
          assertTrue(
            comptime(
              try 10
              finally ()
            ) == 10
          )
        },
        test("finally runs on exception") {
          assertTrue(
            comptime(
              try "x".toInt
              catch case _: NumberFormatException => 5
              finally ()
            ) == 5
          )
        }
      ),
      suite("nested try/catch")(
        test("inner catch doesn't match, outer does") {
          assertTrue(
            comptime(
              try
                try "inner".toInt
                catch case _: NullPointerException => -1
              catch case _: NumberFormatException => 42
            ) == 42
          )
        }
      ),
      suite("multiple catch clauses")(
        test("first matching clause wins") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch
                case _: NumberFormatException    => 1
                case _: IllegalArgumentException => 2 // NFE extends IAE, but first match wins
                case _: Exception                => 3
            ) == 1
          )
        },
        test("second clause matches when first doesn't") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch
                case _: NullPointerException  => 1
                case _: NumberFormatException => 2
                case _: Exception             => 3
            ) == 2
          )
        },
        test("fallback to Exception when specific types don't match") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch
                case _: NullPointerException => 1
                case _: ArithmeticException  => 2
                case _: Exception            => 3
            ) == 3
          )
        }
      ),
      suite("guards")(
        test("guard evaluates to true - clause matches") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch
                case e: NumberFormatException if e.getMessage.contains("abc") => 1
                case _: Exception                                             => 2
            ) == 1
          )
        },
        test("guard evaluates to false - falls through to next clause") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch
                case e: NumberFormatException if e.getMessage.contains("xyz") => 1
                case _: NumberFormatException                                 => 2
                case _: Exception                                             => 3
            ) == 2
          )
        }
        // TODO: test("guard that throws propagates exception") - needs more infrastructure
      ),
      suite("exception type aliases")(
        test("catch scala.IndexOutOfBoundsException matches java.lang.IndexOutOfBoundsException") {
          assertTrue(
            comptime(
              try List(1, 2, 3)(10) // throws IndexOutOfBoundsException
              catch case _: IndexOutOfBoundsException => 42
            ) == 42
          )
        },
        test("catch ArrayIndexOutOfBoundsException") {
          assertTrue(
            comptime(
              try Array(1, 2, 3)(10)
              catch
                case _: ArrayIndexOutOfBoundsException => 42
                case _: IndexOutOfBoundsException      => 43
            ) == 42
          )
        },
        test("catch StringIndexOutOfBoundsException") {
          assertTrue(
            comptime(
              try "abc".charAt(10)
              catch
                case _: StringIndexOutOfBoundsException => 42
                case _: IndexOutOfBoundsException       => 43
            ) == 42
          )
        }
      ),
      suite("by-name contexts")(
        test("try in Option.getOrElse default - Some case should NOT evaluate try") {
          // If Some(1), the getOrElse default should not be evaluated at all
          // This tests that try/catch respects by-name semantics
          assertTrue(
            comptime(
              Option(1).getOrElse {
                try "will throw".toInt
                catch case _: Exception => -999
              }
            ) == 1
          )
        },
        test("try in Option.getOrElse default - None case evaluates try") {
          assertTrue(
            comptime(
              Option.empty[Int].getOrElse {
                try "abc".toInt
                catch case _: NumberFormatException => 42
              }
            ) == 42
          )
        },
        test("try in Either.fold - only evaluated for matching case") {
          assertTrue(
            comptime {
              val e: Either[String, Int] = Right(1)
              e.fold(
                _ =>
                  try "never evaluated".toInt
                  catch case _: Exception => -999,
                identity
              )
            } == 1
          )
        }
      ),
      suite("catch body behavior")(
        test("catch body can do complex computation") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch
                case e: NumberFormatException =>
                  val msg = e.getMessage
                  val len = msg.length
                  len * 2
            ) > 0
          )
        }
      ),
      suite("finally edge cases")(
        test("finally result is discarded, try body result returned") {
          assertTrue(
            comptime(
              try 42
              finally
                val _ = 1 + 2 // result discarded
            ) == 42
          )
        },
        test("finally result is discarded, catch result returned") {
          assertTrue(
            comptime(
              try "abc".toInt
              catch case _: Exception => 99
              finally
                val _ = 1 + 2 // result discarded
            ) == 99
          )
        }
      ),
      suite("exception propagation")(
        test("catch body throws - outer handler catches catch's exception") {
          // When catch body throws, outer try should catch THAT exception
          // not the original NumberFormatException
          // We use List(1)(10) to throw IndexOutOfBoundsException in catch body
          assertTrue(
            comptime(
              try
                val inner: Int =
                  try "abc".toInt
                  catch
                    case _: NumberFormatException =>
                      // Catch matches, but then throws a different exception
                      List(1, 2, 3)(10) // throws IndexOutOfBoundsException
                inner
              catch
                case _: IndexOutOfBoundsException => 1 // catch's exception
                case _: NumberFormatException     => 2 // original exception
                case _: Exception                 => 3
            ) == 1
          )
        },
        test("finally runs even when catch body throws") {
          // Test that finally executes even if catch throws
          // We use nested try to verify the exception propagates correctly
          assertTrue(
            comptime(
              try
                try
                  val inner: Int =
                    try "abc".toInt
                    catch
                      case _: NumberFormatException =>
                        List(1, 2, 3)(10) // throws IndexOutOfBoundsException
                  inner
                finally
                  // This finally should run even though catch threw
                  val _ = 1 + 1
              catch
                case _: IndexOutOfBoundsException => 1 // catch's exception propagated through finally
                case _: Exception                 => 2
            ) == 1
          )
        },
        test("finally exception replaces catch exception") {
          // If finally throws, its exception should replace any prior exception
          // catch throws IndexOutOfBoundsException, finally throws ArithmeticException
          assertTrue(
            comptime(
              try
                val inner: Int =
                  try "abc".toInt
                  catch
                    case _: NumberFormatException =>
                      List(1, 2, 3)(10) // throws IndexOutOfBoundsException
                  finally
                    val _ = 1 / 0 // throws ArithmeticException
                inner
              catch
                case _: ArithmeticException       => 1 // finally's exception wins
                case _: IndexOutOfBoundsException => 2 // catch's exception
                case _: Exception                 => 3
            ) == 1
          )
        },
        test("finally exception replaces successful result") {
          // If finally throws, its exception should replace successful result
          assertTrue(
            comptime(
              try
                try 42
                finally
                  val _ = 1 / 0 // throws ArithmeticException
              catch
                case _: ArithmeticException => -1
                case _: Exception           => -2
            ) == -1
          )
        }
      ),
      suite("explicit throw expressions")(
        test("re-throw caught exception") {
          assertTrue(
            comptime(
              try
                try "abc".toInt
                catch case e: NumberFormatException => throw e
              catch case _: NumberFormatException => 42
            ) == 42
          )
        },
        test("throw after processing exception allows outer handler to catch") {
          // Catch exception, get its message length, then re-throw
          assertTrue(
            comptime(
              try
                try "abc".toInt
                catch
                  case e: NumberFormatException =>
                    val msgLen = e.getMessage.length
                    if msgLen > 0 then throw e
                    else 0
              catch case _: NumberFormatException => 99
            ) == 99
          )
        }
      )
    )
