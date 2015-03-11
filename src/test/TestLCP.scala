import org.scalatest.FunSuite

import scala.annotation.tailrec

class TestLCP extends FunSuite {

  /** @return the prefix string that all of `values` start with, or the empty
    *         string if `values` is empty, or there is no common prefix<br/>
    * ''ensuring'' `result.isEmpty || values.forall(_.startsWith(result))` */
  def commonPrefix(values: String*) = values match {
      case Nil => ""
      case seq => seq.min.zip(seq.max).takeWhile(v => v._1 == v._2).unzip._1.mkString
    }

  test("empty directory path") {
    val path = commonPath(
      "usr/user1/tmp/coverage/test",
      "home/user1/tmp/covert/operator",
      "opt/user1/tmp/coven/members")
    assert(path.isEmpty)
  }

  def commonPairR(first: String, second: String): String = {
    @tailrec def common(word1: String, word2: String, acc: String = ""): String =
      if (word2.nonEmpty && word1.startsWith(word2 take 1)) {
        common(word1 drop 1, word2 drop 1, acc + word1(0))
      } else {
        acc
      }
    common(first, second)
  }

  def commonPair1(path1: String, path2: String) = {
    def truncate(path: String) = {
      val pos = path lastIndexOf '/'
      if (pos > 0 && pos < path.length) path take pos else path
    }
    lazy val prefix = truncate(path1.zip(path2).takeWhile(v => v._1 == v._2).unzip._1.mkString)
    if (path1 == path2) path1 else prefix
  }

  def commonPair(path1: String, path2: String) = if (path1 == path2) {
    path1
  } else {
    val sep = "(?<=/)|(?=/)"
    path1.split(sep).zip(path2.split(sep)).takeWhile(v => v._1 == v._2).unzip._1.mkString
  }

  def commonPath(paths: String*) = paths match {
    case Nil => ""
    case seq => commonPair(seq.min, seq.max)
  }

  test("common directory path") {
    val path = commonPath(
      "/home/user1/tmp/coverage/test",
      "/home/user1/tmp/covert/operator",
      "/home/user1/tmp/coven/members")
    assert(path === "/home/user1/tmp/")
  }

  test("rosetta code tests") {
    //assert(commonPath(test.take(1)) === test.head)
    assert(commonPath(Nil :_*) === "")
    assert(commonPath("") === "")
    assert(commonPath("/") === "/")
    assert(commonPath("/", "") === "")
    assert(commonPath("/", "/a") === "/")
    assert(commonPath("/a", "/b") === "/")
    assert(commonPath("/a", "/a") === "/a")
    assert(commonPath("/a/a", "/b") === "/")
    assert(commonPath("/a/a", "/b") === "/")
    assert(commonPath("/a/a", "/a") === "/a")
    assert(commonPath("/a/a", "/a/b") === "/a/")
    assert(commonPath("/a/b", "/a/b") === "/a/b")
    assert(commonPath("a", "/a") === "")
    assert(commonPath("a/a", "/a") === "")
    assert(commonPath("a/a", "/b") === "")
    assert(commonPath("a", "a") === "a")
    assert(commonPath("a/a", "b") === "")
    assert(commonPath("a/a", "b") === "")
    assert(commonPath("a/a", "a") === "a")
    assert(commonPath("a/a", "a/b") === "a/")
    assert(commonPath("a/b", "a/b") === "a/b")
    assert(commonPath("/a/", "/b/") === "/")
    assert(commonPath("/a/", "/a/") === "/a/")
    assert(commonPath("/a/a/", "/b/") === "/")
    assert(commonPath("/a/a/", "/b/") === "/")
    assert(commonPath("/a/a/", "/a/") === "/a/")
    assert(commonPath("/a/a/", "/a/b/") === "/a/")
    assert(commonPath("/a/b/c", "/a/b/d") === "/a/b/")
    assert(commonPath("/a/b/", "/a/b/") === "/a/b/")
  }

  test("shared start") {
    val testSet2 = List("trains", "trams", "trolleys", "trolls")
    assert(commonPrefix(testSet2 :_*) === "tr")

    assert(commonPrefix("interspecies","interstelar","interstate") === "inters")
    assert(commonPrefix("throne","throne") === "throne")
    assert(commonPrefix("throne","dungeon").isEmpty)
    assert(commonPrefix("cheese") === "cheese")
    assert(commonPrefix("").isEmpty)
    assert(commonPrefix(Nil :_*).isEmpty)
    assert(commonPrefix("prefix","suffix").isEmpty)
  }

}
