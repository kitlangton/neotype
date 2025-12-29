package neotype.interop.upickle

import neotype.*
import neotype.test.*
import neotype.test.definitions.*
import upickle.default.*
import zio.test.*

object UpickleLibrary extends JsonLibrary[ReadWriter]:
  def decode[A](json: String)(using rw: ReadWriter[A]): Either[String, A] =
    try Right(read[A](json))
    catch
      case e: upickle.core.TraceVisitor.TraceException =>
        Left(Option(e.getCause).map(_.getMessage).getOrElse(e.getMessage))
      case e: Exception => Left(e.getMessage)

  def encode[A](value: A)(using rw: ReadWriter[A]): String =
    write(value)

given ReadWriter[Composite]      = macroRW
given ReadWriter[OptionalHolder] = macroRW
given ReadWriter[ListHolder]     = macroRW

object UpickleSpec extends JsonLibrarySpec[ReadWriter]("Upickle", UpickleLibrary):
  override protected def optionalHolderCodec: Option[ReadWriter[OptionalHolder]] =
    Some(summon[ReadWriter[OptionalHolder]])
  override protected def listHolderCodec: Option[ReadWriter[ListHolder]] =
    Some(summon[ReadWriter[ListHolder]])
