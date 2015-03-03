package org.nulleins.values.phone

case class PhoneNumber(number: Long, plan: PhoneNumberPlan) {
  require(plan.valid(number), s"$number must be valid for scheme")
  def country = plan.country
  def countryCode = plan.countryCode
  lazy val areaCode = plan.areaCode(number)
  lazy val subscriberNumber = plan.subscriberNumber(number)
  override def toString =
    s"+$countryCode $areaCode ${subscriberNumber.toString.sliding(4,4).mkString(" ")}"
}
