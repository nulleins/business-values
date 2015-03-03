package org.nulleins.values.phone

case class PhoneNumberFactory(plans: Map[Int, PhoneNumberPlan]) {
  def valid(number: Long) = for {scheme <- findPlan(number)
                                 result <- Some(scheme.valid(number))} yield result

  def findPlan(number: Long) = {
    import PhoneNumberFactory.createKey
    val length = utility.digits(number)
    (for {n <- math.min(utility.digits(number), 4) to 1 by -1
          tryCC = (number / math.pow(10, length - n)).toInt
          key = createKey(tryCC, length)
          result <- plans.get(key)
    } yield result).headOption
  }
}

object PhoneNumberFactory {
  import scala.io.Source
  def apply(configFile: String): PhoneNumberFactory = PhoneNumberFactory(
    (Source.fromInputStream(getClass.getResourceAsStream(configFile)).getLines()
      .filter(!_.startsWith("#")) map (PhoneNumberPlan(_)))
      .foldLeft(Map[Int, PhoneNumberPlan]())((res, scheme) =>
        res + (createKey(scheme.countryCode, scheme.length) -> scheme)))

  def createKey(countryCode: Int, length: Int) = (countryCode << 4) + length-1
}
