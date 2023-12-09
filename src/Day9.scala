import scala.io.Source

object Day9 {
  def part1(input: List[String]): Unit = {
    println(input.map(_.split(" ").toList.map(_.toLong))
      .map(a => a.tail.foldLeft((a, List(a.last)))((acc, cur) => {
        val newRow = acc._1.tail.foldLeft((acc._1.head, List()): (Long, List[Long]))((acc2, cur2) => {
          (cur2, acc2._2 :+ (cur2 - acc2._1))
        })
        (newRow._2, acc._2 :+ newRow._2.last)
      }))
      .map(_._2.sum)
      .sum
    )
  }

  def part2(input: List[String]): Unit = {
    println(input.map(_.split(" ").toList.map(_.toLong))
      .map(a => a.tail.foldLeft((a, List(a.head)))((acc, cur) => {
        val newRow = acc._1.tail.foldLeft((acc._1.head, List()): (Long, List[Long]))((acc2, cur2) => {
          (cur2, acc2._2 :+ (cur2 - acc2._1))
        })
        (newRow._2, acc._2 :+ newRow._2.head)
      }))
      .map(_._2)
      .map(x => x.zip(List.range(0, x.size)).map(y => if (y._2 % 2 == 0) y._1 else -y._1))
      .map(_.sum)
      .sum
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day9.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
