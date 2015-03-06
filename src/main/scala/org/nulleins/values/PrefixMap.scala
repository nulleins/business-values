import scala.annotation.tailrec
import scala.collection.mutable

class PrefixTree(
    val nodes: mutable.Map[Char, PrefixTree] = mutable.Map[Char, PrefixTree]()) {
  var leaf = false

  def insert(words: String*) = words foreach { word =>
    word.foldLeft(this)((next,ch) => {
      next.nodes.getOrElseUpdate(ch, {
        new PrefixTree { next.nodes.put(ch,this)} })
    }).leaf = true
  }

  /** @return longest prefix of `input` in this tree, else an empty string */
  def apply(input: String): String = input take prefix(input)

  /** @return length of the longest prefix matching `input` or zero if no match */
  @tailrec final def prefix(input: String, pos: Int = 0, longest: Int = 0): Int =
    if (input.isEmpty || !nodes.contains(input(0))) {
      if (leaf) pos else longest
    } else {
      nodes(input(0)).prefix(input drop 1, pos + 1, if (leaf) pos else longest)
    }
}

object PrefixTree {
  def apply(words: String*): PrefixTree = new PrefixTree { insert(words :_*)}
  def apply(words: List[String]): PrefixTree = apply(words :_*)
  def unapply(tree: PrefixTree): Option[List[String]] = {
    def extract(node: PrefixTree, head: String = ""): List[String] =
      node.nodes.foldLeft(List[String]()) { case (acc, (k, v)) =>
        acc ++ (if (v.leaf) {
          List(head + k)
        } else {
          List()
        }) ++ extract(v, head + k)
      }
    Some(extract(tree))
  }
}
