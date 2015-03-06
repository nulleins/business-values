import scala.annotation.tailrec
import scala.collection.mutable

private case class PrefixNode(
                               nodes: mutable.Map[Char, PrefixNode] = mutable.Map[Char, PrefixNode]()) {
  var leaf = false
  def this(char: Char, node: PrefixNode) = this(mutable.Map(char -> node))

  @tailrec final def prefix(input: String, pos: Int = 0, longest: Int = 0): Int =
    if (input.isEmpty || !nodes.contains(input(0))) {
      if (leaf) pos else longest
    } else {
      nodes(input(0)).prefix(
        input drop 1, pos + 1, if (leaf) pos else longest)
    }

  def insert(word: String) = word.foldLeft(this)((next,ch) => {
    next.nodes.getOrElseUpdate(ch, new PrefixNode(ch, this))
  }).leaf = true
}

class PrefixTree {
  private val root = new PrefixNode
  private def insertWord(word: String) = root.insert(word)
  def insert(words: String*) = words foreach insertWord
  def apply(input: String): String = input take root.prefix(input)
}

object PrefixTree {
  def apply(words: String*): PrefixTree = {
    val result = new PrefixTree
    result.insert(words :_*)
    result
  }
  def apply(words: List[String]): PrefixTree = apply(words :_*)
}
