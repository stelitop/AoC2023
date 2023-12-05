import scala.io.Source

object Template {
  def part1(input: List[String]): Unit = {

  }

  def part2(input: List[String]): Unit = {

  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/DayXX.txt")
    val input = source.getLines().toList
    source.close()
    part1(input)
    //part2(input)
  }
}
