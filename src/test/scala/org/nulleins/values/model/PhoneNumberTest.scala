import com.citibank.citift.sim.model.ISO3166
import org.nulleins.values.phone.{PhoneNumberFactory, PhoneNumberPlan}
import org.scalatest.FunSuite

class MSISDNTest extends FunSuite {
  val testSpec = "EG.9=2,2,7;CC=20;NDC=10,16,19,11,14,12,17,18"
  val factory = PhoneNumberFactory("/msisdn.properties")

  test("parses test spec correctly") {
    val scheme = PhoneNumberPlan(testSpec)
    assert(scheme.countryCode == 20)
    assert(scheme.length == 11)
    assert(scheme.areaCodes.contains(10))
  }

  test("loads spec file OK") {
    factory.plans.foreach(println)
  }

  test("prefix match") {
    val ieNumber = 353863578380L
    val scheme = factory.findPlan(ieNumber)
    println(scheme)
    assert(factory.valid(ieNumber).isDefined)
    assert(scheme.get.country === ISO3166("IE"))
    assert(scheme.get.length === 12)
    assert(scheme.get.areaCodes === Set(88, 89, 85, 86, 87, 82, 83))
    assert(scheme.get.countryCode(ieNumber) === 353)
    assert(scheme.get.areaCode(ieNumber) === 86)
    assert(scheme.get.subscriberNumber(ieNumber) === 3578380)
    val scheme2 = factory.findPlan(352212345)
    println(scheme2)
  }

  test("create phone number") {
    val number = 353863578380L
    val scheme = factory.findPlan(number)
    val msisdn = scheme.get.create(number)
    println(msisdn)
  }

  test("no registered scheme") {
    val frNumber = 3380768726236L
    assert(factory.findPlan(frNumber).isEmpty)
  }

}
