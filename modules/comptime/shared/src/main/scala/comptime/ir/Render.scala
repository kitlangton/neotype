package comptime

private[comptime] object Render:
  def show(call: CallIR): String =
    s"${call.owner}.${call.name}"
