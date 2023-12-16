import scala.io.Source

object Day15 {

  def hash(s: String): Int = {
    s.toCharArray.foldLeft(0)((acc, cur) => ((cur + acc)*17)%256)
  }

  def part1(input: List[String]): Unit = {
    println(input.map(hash).sum)
  }

  def part2(input: List[String]): Unit = {
    input.foldLeft(Map(): Map[Int, List[(String, Int)]])((acc, cur) => {
      if (cur.reverse(1) == '=') {
        val s = cur.substring(0, cur.length - 2)
        val h = hash(s)
        if (!acc.contains(h)) return acc + (h -> )
        return acc + (acc.getOrElse(h, Map()) + (s -> cur.reverse(0).asDigit))
      } else {
        val s = cur.substring(0, cur.length - 1)
        val h = hash(s)
        return acc + (acc.getOrElse(h, Map()) - s)
      }
    }).mapValues()
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day15.txt")
    val input = source.getLines().toList.head.split(",").toList
    source.close()
    part1(input)
    //part2(input)
  }
}
