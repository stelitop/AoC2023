import scala.io.Source

object Day1 {

  def part1(input: List[String]): Unit = {
    val looked = List(("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9))
    println(
      input.map(x => (x, looked.map(y => (y, x.indexOf(y._1), x.lastIndexOf(y._1))).filter(y => y._2 != -1)))
      .map(x => (x._1, x._2.minBy(y => y._2)._1._2, x._2.maxBy(y => y._3)._1._2))
      .map(x => x._2 * 10 + x._3)
      .sum
    )
  }

  def part2(input: List[String]): Unit = {
    val looked = List(("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9),
      ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9))

    println(
      input.map(x => (x, looked.map(y => (y, x.indexOf(y._1), x.lastIndexOf(y._1))).filter(y => y._2 != -1)))
        .map(x => (x._1, x._2.minBy(y => y._2)._1._2, x._2.maxBy(y => y._3)._1._2))
        .map(x => x._2 * 10 + x._3)
        .sum
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day1.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
