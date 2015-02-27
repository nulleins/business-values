import com.citibank.citift.sim.model.IBANScheme._
import com.citibank.citift.sim.model._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IBANSchemeTest extends FunSuite {

  val ibanBadChecksum = "ZA001234987601234567890098ABCD"
  val ibanZAGood = "ZA98-1234-9876-01234-567890098/ABCD"
  val ibanKGGood = "KG73 05401 051 80021273113007"

  test("create IBANScheme") {
    val schemeZA = IBANScheme(ISO3166("ZA"), "4n", "4n", "18c")
    val schemeKG = IBANScheme("KG: 5n 3n 14n")
    assert(schemeZA.length === 30)
    assert(!schemeZA.valid(ibanBadChecksum))
    val factory = IBANFactory(schemeZA, schemeKG)
    assert(factory.schemeFor(ISO3166("ZA")).isSuccess)

    schemeZA.parse(ibanBadChecksum) fold (
      failure => assert(failure === "Invalid checksum"),
      success => fail())
    assert(generateChecksum(ISO3166("KG"), "0540105180021273113007") === 73)
    assert(schemeKG.valid(ibanKGGood))
    assert(schemeZA.valid(ibanZAGood))
    val subject = factory.create(ibanZAGood)
    assert(subject.countryCode === ISO3166("ZA"))
    assert(subject.bankCode === "1234")
    assert(subject.branchCode === "9876")
    assert(subject.accountNumber === "01234567890098ABCD")
    assert(subject.bban === BBAN("1234987601234567890098ABCD"))
    assert(subject.formatted === "ZA98 1234 9876 0123 4567 8900 98AB CD")
    assert(subject.toString === "ZA981***********************CD")
    assert(subject.value === "ZA981234987601234567890098ABCD")
  }

}
