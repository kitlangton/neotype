package neotype

given CustomFailureNewtype: Newtype[String] with
  inline def validate(value: String): Boolean =
    value == "secret string"

  override inline def failureMessage: String = "Must be the secret string!"

// TODO: Figure out why the compiler won't let me override an inline method with a default member
given BadCustomFailureNewtype: Newtype[String] with
  inline def validate(value: String): Boolean =
    value == "secret string"

  override def failureMessage: String = "Oops. I forgot to inline"

given AnotherCustomFailureNewtype: Newtype[String] with
  inline def validate(value: String): Boolean =
    value.trim.length > 10

  override inline def failureMessage: String = "Must be the secret string!"

object Use extends App:
  AnotherCustomFailureNewtype("secret string")
