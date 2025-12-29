package comptime.util

import comptime.ComptimeError
import comptime.Eval
import comptime.TermIR

private[comptime] object Args:
  def compileArgsList(
      args: List[TermIR],
      compile: TermIR => Either[ComptimeError, Eval]
  ): Either[ComptimeError, List[Eval]] =
    args.foldLeft(Right(List.empty[Eval]): Either[ComptimeError, List[Eval]]) { (acc, term) =>
      for
        list <- acc
        eval <- compile(term)
      yield list :+ eval
    }
