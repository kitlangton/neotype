package neotype.interop.upickle

import neotype.*
import neotype.test.*
import neotype.test.definitions.*
import upickle.default.*
import zio.test.*

object UpickleLibrary extends JsonLibrary[ReadWriter]:
  def decode[A](json: String)(using rw: ReadWriter[A]): Either[String, A] =
    try Right(read[A](json))
    catch case e: Exception => Left(e.getMessage)

  def encode[A](value: A)(using rw: ReadWriter[A]): String =
    write(value)

given compositeCodec: ReadWriter[Composite] = macroRW
object UpickleSpec extends JsonLibrarySpec[ReadWriter]("Upickle", UpickleLibrary)
