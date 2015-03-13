In Scala:

    /** @return prefix string that all of `list` start with, or the empty
      *         string if `list` is empty, or there is no common prefix
      *   ensure: result.isEmpty || list.forall(_.startsWith(result)) */
    def commonPrefix(list: String*) = list.foldLeft("")((_,_)=>
      (list.min zip list.max).takeWhile(v => v._1 == v._2).unzip._1.mkString)

Test case:

    test("shared start") {
      commonPrefix("interspecies","interstelar","interstate") shouldBe "inters"
    }
