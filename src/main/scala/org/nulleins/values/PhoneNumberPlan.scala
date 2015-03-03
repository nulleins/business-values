package org.nulleins.values.phone

import com.citibank.citift.sim.model.ISO3166
import org.nulleins.values.phone.utility._

case class PhoneNumberPlan(countryCode: Int, country: ISO3166, sizes: (Int,Int,Int), areaCodes: Set[Int]) {
  protected[phone] lazy val ccfactor = math.pow(10, sizes._2 + sizes._3)
  protected[phone] lazy val ndcfactor = math.pow(10, sizes._3)
  lazy val length = sizes._1 + sizes._2 + sizes._3

  def valid(number: Long) = countryCode(number) == countryCode &&
    (areaCodes.isEmpty || areaCodes.contains(areaCode(number))) && digits(number) == length

  def countryCode(number: Long) = (number / ccfactor).toInt
  def areaCode(number: Long) = ((number - (countryCode * ccfactor)) / ndcfactor).toInt
  def subscriberNumber(number: Long) = (number - (countryCode * ccfactor)
    - (areaCode(number) * ndcfactor)).toInt

  def create(number: Long) = PhoneNumber(number,this)
}

object PhoneNumberPlan {
  private val specPattern = """([A-Z]{2}[A-Z0-9\.]*)=(\d+,\d+,\d+);CC=(\d+)(;NDC=\d+[,\d+]*)?""".r
  private val ccPattern = """([A-Z]{2})([A-Z0-9\.]+)?""".r
  private val ndcPattern = """;(NDC)=(\d+[,\d+]*)""".r

  def apply(plan: String): PhoneNumberPlan = plan match {
      case specPattern(key, sizes, cc, ndc) =>
        val Array(ccLen, ndcLen, snLen) = sizes.split(",")
        val country = key match { case ccPattern(isocc,_) => ISO3166(isocc)}
        PhoneNumberPlan(cc.toInt, country, (ccLen.toInt,ndcLen.toInt,snLen.toInt), getNdc(ndc))
      case _ => throw new RuntimeException(s"cannot parse phone numbering plan: $plan")
    }

  private def getNdc(ndc: String) = ndc match {
    case ndcPattern(_, list) => list.split(",").map(_.toInt).toSet
    case _ => Set[Int]()
  }
}
