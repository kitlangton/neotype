package neotype

import zio.test.*

import scala.reflect.ClassTag

/** Tests verifying that newtypes are truly zero-overhead at runtime.
  *
  * Neotype uses Scala 3 opaque types which should:
  *   1. Have no wrapper objects (the newtype IS the underlying type at runtime)
  *   2. Preserve primitive types (Int stays int, not Integer)
  *   3. Use primitive array storage (Array[Newtype[Int]] uses int[])
  */
object ZeroOverheadSpec extends ZIOSpecDefault:

  // Test fixtures - simple newtypes for primitives
  type WrappedInt = WrappedInt.Type
  object WrappedInt extends Newtype[Int]

  type WrappedLong = WrappedLong.Type
  object WrappedLong extends Newtype[Long]

  type WrappedDouble = WrappedDouble.Type
  object WrappedDouble extends Newtype[Double]

  type WrappedString = WrappedString.Type
  object WrappedString extends Newtype[String]

  // Subtype fixtures
  type SubInt = SubInt.Type
  object SubInt extends Subtype[Int]

  val spec = suite("ZeroOverheadSpec")(
    suite("Newtype runtime type identity")(
      test("Newtype[Int] is Int at runtime") {
        val wrapped: WrappedInt = WrappedInt(42)
        // At runtime, wrapped IS an Int, so isInstanceOf[Int] should be true
        // Note: This boxes the Int, but the underlying value is still primitive
        val isInt = wrapped.asInstanceOf[Any].isInstanceOf[Int]
        assertTrue(isInt)
      },
      test("Newtype[Long] is Long at runtime") {
        val wrapped: WrappedLong = WrappedLong(42L)
        val isLong               = wrapped.asInstanceOf[Any].isInstanceOf[Long]
        assertTrue(isLong)
      },
      test("Newtype[Double] is Double at runtime") {
        val wrapped: WrappedDouble = WrappedDouble(3.14)
        val isDouble               = wrapped.asInstanceOf[Any].isInstanceOf[Double]
        assertTrue(isDouble)
      },
      test("Newtype[String] is String at runtime") {
        val wrapped: WrappedString = WrappedString("hello")
        val isString               = wrapped.asInstanceOf[Any].isInstanceOf[String]
        assertTrue(isString)
      }
    ),
    suite("Subtype runtime type identity")(
      test("Subtype[Int] is Int at runtime") {
        val wrapped: SubInt = SubInt(42)
        val isInt           = wrapped.asInstanceOf[Any].isInstanceOf[Int]
        assertTrue(isInt)
      },
      test("Subtype is directly assignable to base type") {
        val wrapped: SubInt = SubInt(42)
        val asInt: Int      = wrapped // No cast needed - this IS the zero-overhead proof
        assertTrue(asInt == 42)
      }
    ),
    suite("Primitive array storage")(
      test("Array[Newtype[Int]] uses primitive int[] storage") {
        val arr: Array[WrappedInt] = Array(WrappedInt(1), WrappedInt(2), WrappedInt(3))
        // If zero-overhead, component type should be int (primitive), not Integer
        val componentType = arr.getClass.getComponentType
        assertTrue(componentType == classOf[Int])
      },
      test("Array[Newtype[Long]] uses primitive long[] storage") {
        val arr: Array[WrappedLong] = Array(WrappedLong(1L), WrappedLong(2L))
        val componentType           = arr.getClass.getComponentType
        assertTrue(componentType == classOf[Long])
      },
      test("Array[Newtype[Double]] uses primitive double[] storage") {
        val arr: Array[WrappedDouble] = Array(WrappedDouble(1.0), WrappedDouble(2.0))
        val componentType             = arr.getClass.getComponentType
        assertTrue(componentType == classOf[Double])
      },
      test("Array[Subtype[Int]] uses primitive int[] storage") {
        val arr: Array[SubInt] = Array(SubInt(1), SubInt(2), SubInt(3))
        val componentType      = arr.getClass.getComponentType
        assertTrue(componentType == classOf[Int])
      }
    ),
    suite("ClassTag preservation")(
      test("ClassTag[Newtype[Int]] equals ClassTag[Int]") {
        val newtypeTag = summon[ClassTag[WrappedInt]]
        val intTag     = summon[ClassTag[Int]]
        assertTrue(newtypeTag.runtimeClass == intTag.runtimeClass)
      },
      test("ClassTag[Newtype[String]] equals ClassTag[String]") {
        val newtypeTag = summon[ClassTag[WrappedString]]
        val stringTag  = summon[ClassTag[String]]
        assertTrue(newtypeTag.runtimeClass == stringTag.runtimeClass)
      }
    ),
    suite("No boxing in collections")(
      test("List operations preserve unboxed semantics") {
        // This tests that operations on List[Newtype[Int]] work like List[Int]
        val list: List[WrappedInt] = List(WrappedInt(1), WrappedInt(2), WrappedInt(3))
        // The sum operation works because at runtime these ARE Ints
        val summed = list.asInstanceOf[List[Int]].sum
        assertTrue(summed == 6)
      },
      test("Vector operations preserve unboxed semantics") {
        val vec: Vector[WrappedInt] = Vector(WrappedInt(10), WrappedInt(20))
        val summed                  = vec.asInstanceOf[Vector[Int]].sum
        assertTrue(summed == 30)
      }
    ),
    suite("Identity guarantees")(
      test("unsafeMake returns the exact same reference for reference types") {
        val original               = "hello"
        val wrapped: WrappedString = WrappedString.unsafeMake(original)
        // Should be the SAME object, not a copy
        assertTrue(wrapped.asInstanceOf[AnyRef] eq original)
      },
      test("unwrap returns the exact same reference for reference types") {
        val original               = "world"
        val wrapped: WrappedString = WrappedString.unsafeMake(original)
        val unwrapped              = wrapped.unwrap
        assertTrue(unwrapped eq original)
      }
    )
  )
