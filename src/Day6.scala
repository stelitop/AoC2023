import scala.io.Source

object Day6 {
  def part1(input: List[String]): Unit = {
    println(input.map(x => (x.split(" ")(0).toLong, x.split(" ")(1).toLong))
      .map(x => List.range(0, x._1 + 1).map(y => y * (x._1 - y)).count(y => y > x._2))
      .product)
  }

  def part2(input: List[String]): Unit = {
    val x = input.map(x => (x.split(" ")(0), x.split(" ")(1)))
      .foldLeft(("", ""))((acc, cur) => (acc._1 + cur._1, acc._2 + cur._2))
    val time = x._1.toLong
    val dist = x._2.toLong
    println(List.range(0, time + 1).map(y => y * (time - y)).count(y => y > dist))
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day6.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
