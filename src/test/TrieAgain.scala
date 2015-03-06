import org.scalatest.FunSuite

class TestAgain extends FunSuite {

  test("create and find") {
    println()
    val dict = new TrieMap
    dict.insert("are")
    dict.insert("area")
    dict.insert("base")
    dict.insert("cat")
    dict.insert("cater")
    dict.insert("basement")

    var input = "caterer"
    println(s"$input: ${dict(input)}")

    input = "basement"
    println(s"$input: ${dict(input)}")

    input = "are"
    println(s"$input: ${dict(input)}")

    input = "arenaceous"
    println(s"$input: ${dict(input)}")

    input = "basemexz"
    println(s"$input: ${dict(input)}")

    input = "xyz"
    println(s"$input: ${dict(input)}")
  }

  test("phone numbers") {
    println()
    val dict = TrieMap(
      "30","31","32","33","34","350","351","352",
      "353","354","355","356","357","358","35818")

    println(s"""353963578380: ${dict("353963578380")}""")
    println(s"""448183578380: ${dict("448183578380")}""")
    println(s"""3019292929121: ${dict("3019292929121")}""")
    println(s"""3581883287288: ${dict("3581883287288")}""")
  }
}
