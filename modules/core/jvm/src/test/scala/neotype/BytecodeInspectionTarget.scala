package neotype

/** Target class for bytecode inspection to verify zero overhead. Compile this
  * and inspect with: javap -c -classpath ... neotype.BytecodeInspectionTarget
  *
  * Expected: unsafeMake/unwrap calls should NOT appear in bytecode. The methods
  * should inline to no-ops.
  */
object BytecodeInspectionTarget:
  type UserId = UserId.Type
  object UserId extends Newtype[Int]

  // These should compile to just returning the input value
  def testUnsafeMake(n: Int): UserId = UserId.unsafeMake(n)
  def testUnwrap(id: UserId): Int    = id.unwrap

  // This should compile to just the validation + return
  def testMake(n: Int): Either[String, UserId] = UserId.make(n)

  // This is a baseline - should be identical bytecode to testUnsafeMake
  def baseline(n: Int): Int = n
