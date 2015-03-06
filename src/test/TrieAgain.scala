import org.scalatest.FunSuite

class PrefixTreeTest extends FunSuite {

   test("create and find") {
     println()
     val dict = PrefixTree("are","area","arena","base","cat","cater","basement")

     assert(dict("caterer") === "cater")
     assert(dict("basement") === "basement")
     assert(dict("are") === "are")
     assert(dict("arenaceous") === "arena")
     assert(dict("basemexz") === "base")
     assert(dict("xyz").isEmpty)
   }

   test("phone numbers") {
     println()
     val dict = PrefixTree(List(
       "30","31","32","33","34","350","351","352",
       "353","354","355","356","357","358","35818"))

     println(s"""353963578380: ${dict("353963578380")}""")
     println(s"""448183578380: ${dict("448183578380")}""")
     println(s"""3019292929121: ${dict("3019292929121")}""")
     println(s"""3581883287288: ${dict("3581883287288")}""")
     println(s""": ${dict("")}""")
     println(s"""3: ${dict("3")}""")
     println(s"""35: ${dict("35")}""")
     println(s"""350: ${dict("350")}""")
     println(s"""3501: ${dict("3501")}""")

     assert(dict("353963578380") === "353")
     assert(dict("448183578380").isEmpty)
     assert(dict("3019292929121") === "30")
     assert(dict("3581883287288") === "35818")
     assert(dict("").isEmpty)
     assert(dict("3").isEmpty)
     assert(dict("35").isEmpty)
     assert(dict("350") === "350")
     assert(dict("3501") === "350")
   }
 }
