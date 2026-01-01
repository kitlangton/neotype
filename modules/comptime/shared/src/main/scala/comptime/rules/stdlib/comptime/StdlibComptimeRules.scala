// Stdlib rules for comptime.* helpers (hand-maintained).
package comptime

import java.nio.charset.Charset
import java.nio.charset.IllegalCharsetNameException
import java.nio.charset.UnsupportedCharsetException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

private[comptime] object StdlibComptimeRules:
  private val comptimeRecv    = Recv.module("comptime.package")
  private val defaultEncoding = "UTF-8"

  private def baseDir(pos: Option[SourcePos]): Option[Path] =
    pos.flatMap { p =>
      val path = Paths.get(p.file)
      Option(path.getParent)
    }

  private def basePath(base: Option[Path]): Path =
    base.getOrElse(Paths.get("").toAbsolutePath.normalize())

  private def resolvePath(path: String, base: Option[Path]): Path =
    val raw = Paths.get(path)
    if raw.isAbsolute then raw.normalize()
    else basePath(base).resolve(path).normalize()

  private def describeBase(base: Option[Path]): String =
    base.map(_.toString).getOrElse(s"${basePath(base)} (source file unavailable)")

  private def fileNotFoundMessage(opName: String, path: String, resolved: Path, base: Option[Path]): String =
    s"$opName: file not found: $resolved (requested: $path; base: ${describeBase(base)}; paths are resolved relative to the source file)"

  private def notAFileMessage(opName: String, path: String, resolved: Path, base: Option[Path]): String =
    s"$opName: not a file: $resolved (requested: $path; base: ${describeBase(base)}; paths are resolved relative to the source file)"

  private def readBytes(path: String, base: Option[Path], opName: String): Array[Byte] =
    val resolved = resolvePath(path, base)
    if !Files.exists(resolved) then throw new RuntimeException(fileNotFoundMessage(opName, path, resolved, base))
    if !Files.isRegularFile(resolved) then throw new RuntimeException(notAFileMessage(opName, path, resolved, base))
    Files.readAllBytes(resolved)

  private def charsetFor(name: String): Charset =
    val normalized = if name == null || name.trim.isEmpty then defaultEncoding else name
    try Charset.forName(normalized)
    catch
      case _: IllegalCharsetNameException =>
        throw new RuntimeException(s"comptime.readFile: invalid encoding '$normalized'")
      case _: UnsupportedCharsetException =>
        throw new RuntimeException(s"comptime.readFile: unsupported encoding '$normalized'")

  private def readString(path: String, encoding: String, base: Option[Path]): String =
    val bytes = readBytes(path, base, "comptime.readFile")
    new String(bytes, charsetFor(encoding))

  private val readFileRule: CallRule =
    RuleDsl
      .rule("readFile")
      .recv(comptimeRecv)
      .a1
      .compile("comptime.readFile") { (call, ctx) =>
        call.args1 match
          case Some(pathTerm) =>
            val base = baseDir(call.pos)
            ctx.compileTerm(pathTerm).map { pathEval =>
              pathEval match
                case Eval.Value(value) if ctx.foldConstants =>
                  Eval.Value(readString(value.asInstanceOf[String], defaultEncoding, base))
                case _ =>
                  Eval.BuildList(
                    List(pathEval),
                    values => readString(values.head.asInstanceOf[String], defaultEncoding, base)
                  )
            }
          case None =>
            Left(ComptimeError.UnsupportedArity("readFile", ""))
      }

  private val readFileWithEncodingRule: CallRule =
    RuleDsl
      .rule("readFile")
      .recv(comptimeRecv)
      .a2
      .compile("comptime.readFile") { (call, ctx) =>
        call.args2 match
          case Some((pathTerm, encodingTerm)) =>
            val base = baseDir(call.pos)
            for
              pathEval     <- ctx.compileTerm(pathTerm)
              encodingEval <- ctx.compileTerm(encodingTerm)
            yield (pathEval, encodingEval) match
              case (Eval.Value(path), Eval.Value(enc)) if ctx.foldConstants =>
                Eval.Value(readString(path.asInstanceOf[String], enc.asInstanceOf[String], base))
              case _ =>
                Eval.BuildList(
                  List(pathEval, encodingEval),
                  values => readString(values.head.asInstanceOf[String], values(1).asInstanceOf[String], base)
                )
          case None =>
            Left(ComptimeError.UnsupportedArity("readFile", ""))
      }

  private val readFileBytesRule: CallRule =
    RuleDsl
      .rule("readFileBytes")
      .recv(comptimeRecv)
      .a1
      .compile("comptime.readFileBytes") { (call, ctx) =>
        call.args1 match
          case Some(pathTerm) =>
            val base = baseDir(call.pos)
            ctx.compileTerm(pathTerm).map { pathEval =>
              pathEval match
                case Eval.Value(value) if ctx.foldConstants =>
                  Eval.Value(readBytes(value.asInstanceOf[String], base, "comptime.readFileBytes"))
                case _ =>
                  Eval.BuildList(
                    List(pathEval),
                    values => readBytes(values.head.asInstanceOf[String], base, "comptime.readFileBytes")
                  )
            }
          case None =>
            Left(ComptimeError.UnsupportedArity("readFileBytes", ""))
      }

  val rules: List[CallRule] =
    List(readFileRule, readFileWithEncodingRule, readFileBytesRule)
