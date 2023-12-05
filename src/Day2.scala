import scala.io.Source

object Day2 {
  def part1(input: List[String]): Unit = {
    println(
      input
      .map(x => (x.split(":")(0).split(" ")(1), x.split(":")(1)))
      .map(x => (x._1, x._2.split(";").map(y => y.trim)))
      .map(x => (x._1, x._2.map(y => y
        .split(",")
        .map(z => z.trim)
        .map(z => (z.split(" ")(1), z.split(" ")(0).toInt))
        .foldLeft((0, 0, 0))((acc, z) => z._1 match {
          case "red" => (acc._1 + z._2, acc._2, acc._3)
          case "green" => (acc._1, acc._2 + z._2, acc._3)
          case "blue" => (acc._1, acc._2, acc._3 + z._2)
        })
      )))
        .filter(x => x._2.forall(y => y._1 <= 12 && y._2 <= 13 && y._3 <= 14))
        .map(x => x._1.toInt)
        .sum
    )
  }

  def part2(input: List[String]): Unit = {
    println(
      input
        .map(x => (x.split(":")(0).split(" ")(1), x.split(":")(1)))
        .map(x => (x._1, x._2.replace(";", ",")))
        .map(x => (x._1, x._2.split(",")
          .map(z => z.trim)
          .map(z => (z.split(" ")(1), z.split(" ")(0).toInt))
          .foldLeft((0, 0, 0))((acc, z) => z._1 match {
            case "red" => (Math.max(acc._1, z._2), acc._2, acc._3)
            case "green" => (acc._1, Math.max(acc._2, z._2), acc._3)
            case "blue" => (acc._1, acc._2, Math.max(acc._3, z._2))
          })
        ))
        .map(x => x._2._1 * x._2._2 * x._2._3)
        .sum
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day2.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
