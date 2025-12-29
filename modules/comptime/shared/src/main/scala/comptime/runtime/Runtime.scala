package comptime

object Runtime:
  def notImplemented(name: String): Nothing =
    throw new NotImplementedError(name)
