import scala.annotation.tailrec
import scala.collection.mutable

case class TrieMapNode(
    nodes: mutable.Map[Char, TrieMapNode] = mutable.Map[Char, TrieMapNode]()) {
  var leaf = false

  @tailrec final def prefix(input: String, pos: Int = 0, longest: Int = 0): Int =
    if (input.isEmpty || !nodes.contains(input(0))) {
      if (leaf) pos else longest
    } else {
      nodes(input(0)).prefix(
        input drop 1, pos + 1, if (leaf) pos else longest)
    }
}

class TrieMap {
  val root = new TrieMapNode

  def insertWord(word: String) = {
    var next = root
    word foreach { ch =>
      val child = next.nodes
      if (child.contains(ch)) {
        next = child(ch)
      } else {
        val node = new TrieMapNode
        child.put(ch, node)
        next = node
      }
    }
    next.leaf = true
  }

  def insert(words: String*) = words foreach insertWord

  def apply(input: String) = input take root.prefix(input)
}

object TrieMap {
  def apply(words: String*): TrieMap = {
    val result = new TrieMap
    result.insert(words :_*)
    result
  }
  def apply(words: List[String]): TrieMap = apply(words :_*)
}
