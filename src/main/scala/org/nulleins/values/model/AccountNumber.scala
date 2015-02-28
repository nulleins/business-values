package org.nulleins.values.model
import scalaz._

trait AccountNumber {
  val noise = """[\s\.,_/:-]""".r

  /** @return a Validation of the supplied `code` with non-significant characters removed */
  def normalize(code: String, min: Int): Validation[String, String] = {
    if (code == null) {
      Failure("Code may not be null")
    } else {
      val result = noise.replaceAllIn(code, "").toUpperCase
      if (result.length >= min) Success(result) else Failure( s"""Length of "$result" less than $min""")
    }
  }

  private val stars: Int => String = Memo.mutableHashMapMemo[Int, String] { num =>
    (for (_ <- 1 to num) yield "*").mkString
  }

  private def inRangeOrElse(value: Int, default: Int = 1)(implicit range: Range) =
    if (range contains value) value else default

  /** @return a copy of the supplied account number, with all but the
    *         specified initial and final portion beclouded by stars
    * @param value of the account number
    * @param first length to be displayed in clear
    * @param last length to be displayed in clear */
  def obfuscate(value: String, first: Int, last: Int) = {
    implicit val range = 0 until value.length
    val prefix = inRangeOrElse(first)
    val suffix = inRangeOrElse(last)
    val cloud = stars(math.min(value.length - prefix - suffix, value.length))
    s"${value take prefix}$cloud${value drop value.length - suffix}"
  }
}
