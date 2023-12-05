import scala.io.Source


object Day4 {
  def part1(input: List[String]): Unit = {
    println(input.map(x => x.split(":")(1).trim)
      .map(x => (x.split("\\|")(0).trim.split(" ").toList.filter(y => y.nonEmpty), x.split("\\|")(1).trim.split(" ").toList.filter(y => y.nonEmpty)))
      .map(x => x._1.count(y => x._2.contains(y)))
      .filter(x => x != 0)
      .map(x => Math.pow(2, x - 1))
      .sum)
  }

  case class Ans2(ans: Int, mult: List[Int])

  def part2(input: List[String]): Unit = {
    println(input.map(x => x.split(":")(1).trim)
      .map(x => (x.split("\\|")(0).trim.split(" ").toList.filter(y => y.nonEmpty), x.split("\\|")(1).trim.split(" ").toList.filter(y => y.nonEmpty)))
      .map(x => x._1.count(y => x._2.contains(y)))
      .foldLeft(Ans2(0, List.fill(input.size)(1)))((acc, cur) => {
        val added = acc.mult.head
        val newList = acc.mult.tail.zip(List.range(0, acc.mult.tail.size)).map(x => if (x._2 < cur) x._1 + added else x._1)
        Ans2(acc.ans + added, newList)
      }).ans
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day4.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
