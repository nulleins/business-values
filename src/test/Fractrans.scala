import org.scalatest.FunSuite

import scala.util.matching.Regex.Match

class TestFractan extends FunSuite {
  test("find first fifteen fractans figures") {
    val f = Fractran("17/91 78/85 19/51 23/38 29/33 77/29 95/23 77/19 1/17 11/13 13/11 15/14 15/2 55/1")
    println(f.execute(2) take 20 toList)
  }
}
object Fractran {
  val pattern = """\s*(\d+)\s*/\s*(\d+)\s*""".r
  def parse(m: Match) = ((m group 1).toInt, (m group 2).toInt)
  def apply(program: String) = {
    val (num,den) = pattern.findAllMatchIn(program).map(parse).toList.unzip
    new Fractran(num,den)
  }
}

class Fractran(val num: List[Int], val den: List[Int]) {
  def execute(value: Int) = {
    def step(v: Int) = den.indexWhere(v % _ == 0) match {
      case -1 => None
      case index => Some(num(index) * v / den(index))
    }
    
    var counter = value
    Stream.continually{counter = step(counter).get; counter}
/*  for {
      next <- Stream.from(value)
      counter <- step(next)
    } yield counter*/
  }

}
