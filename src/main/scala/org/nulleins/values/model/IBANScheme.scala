package org.nulleins.values.model

import scala.io.Source
import scalaz.syntax.std.option._
import scalaz._

case class IBANScheme(countryCode: ISO3166, bankPic: String, branchPic: String, accountPic: String)
    extends AccountNumber {
  val pattern = s"^([A-Z]{2})([0-9]{2})${map(bankPic)}${map(branchPic)}${map(accountPic)}$$".r

  def countryCode(value: String) = ISO3166(value take 2)
  def bankCode(value: String) = value match { case pattern(_, _, a, _*) => a }
  def branchCode(value: String) = value match { case pattern(_, _, _, b, _) => b }
  def accountNumber(value: String) = value match { case pattern(_, _, _, _, c) => c }

  /** @return the input `value`, normalized and wrapped in `Success` if it matches the
    *         scheme pattern and the checksum is valid, otherwise a `Failure` wrapping
    *         the failure message detailing the reason it was not considered valid */
  def parse(value: String): Validation[String, String] = for {
    normal <- normalize(value, 5)
    result <- pattern.findFirstIn(normal).toSuccess(s"$value does not match pattern")
    _ <- IBANScheme.validateChecksum(normal)
  } yield result

  def valid(value: String) = parse(value).isSuccess

  def create(value: String): IBAN = parse(value) fold(
    failure => throw new RuntimeException(failure),
    success => IBAN(success, this))

  lazy val length = Seq(bankPic, branchPic, accountPic).foldLeft(4)((acc, part) => {
    part match { case specPattern(n, _) => acc + n.toInt; case _ => 0} })

  private lazy val specPattern = "(\\d+)([AaNnCc])".r
  private lazy val typeMap = Map("A" -> "A-Za-z", "N" -> "\\d", "C" -> "A-Za-z\\d")
  private def map(spec: String) = {
    spec match { case specPattern(n, t) => s"([${typeMap(t.toUpperCase)}]{$n})"; case _ => ""} }
}

object IBANScheme {
  val configPattern = """\s*([A-Z]{2})\s*[=:]\s*(\d+[ANC])\s*(\d+[ANC])\s*(\d+[ANC])\s*""".r
  def apply(spec: String): IBANScheme = spec.toUpperCase match {
    case configPattern(cc, bank, branch, account) => IBANScheme(ISO3166(cc), bank, branch, account)
    case _ => throw new RuntimeException(s""""$spec"" is not a valid scheme specification""")
  }
  def validateChecksum(code: String) = if (checksumValid(code)) Success(code) else Failure("Invalid checksum")
  def generateChecksum(cc: ISO3166, bban: String) = 98 - mod97of(s"${cc}00$bban")
  def checksumValid(code: String) = mod97of(code) == BigInt(1)
  private def mod97of(code: String) = BigInt(((code drop 4) + (code take 4)).map { cc =>
    if (cc isDigit) cc.toInt - '0' else cc - 'A' + 10 }.mkString) mod 97
}

class IBANFactory(schemes: Map[ISO3166,IBANScheme]) {
  implicit def getISO3166(value : String): ISO3166 = ISO3166(if (value != null) value.trim take 2 else "")
  def schemeFor(country: ISO3166) = schemes.get(country).toSuccess(s"""Scheme not registered for "$country"""")
  def create(value: String): IBAN = schemeFor(value) fold (
    failure => throw new RuntimeException(failure),
    success => success.create(value))
  def valid(value: String) = schemeFor(value) fold (failure => false, success => success.valid(value))
}

object IBANFactory {
  def apply(values: IBANScheme*): IBANFactory = new IBANFactory(values.map(v => v.countryCode -> v).toMap)
  def apply(configFile: String): IBANFactory = IBANFactory(
    (Source.fromInputStream(getClass.getResourceAsStream(configFile)).getLines()
      .filter(!_.startsWith("#")) map(IBANScheme(_))).toSeq :_*)
}
