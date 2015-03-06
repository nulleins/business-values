import scala.annotation.tailrec
import scala.collection.mutable

case class PrefixTree(
    nodes: mutable.Map[Char, PrefixTree] = mutable.Map[Char, PrefixTree]()) {
  var leaf = false

  @tailrec final def prefix(input: String, pos: Int = 0, longest: Int = 0): Int =
    if (input.isEmpty || !nodes.contains(input(0))) {
      if (leaf) pos else longest
    } else {
      nodes(input(0)).prefix(input drop 1, pos + 1, if (leaf) pos else longest)
    }

  def insertWord(word: String) = word.foldLeft(this)((next,ch) => {
    next.nodes.getOrElseUpdate(ch, {
      new PrefixTree { next.nodes.put(ch,this)} })
  }).leaf = true
  def insert(words: String*) = words foreach insertWord
  def apply(input: String): String = input take prefix(input)
}

object PrefixTree {
  def apply(words: String*): PrefixTree = new PrefixTree { insert(words :_*)}
  def apply(words: List[String]): PrefixTree = apply(words :_*)
}
