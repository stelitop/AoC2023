import scala.io.Source

object Day5 {

  case class Mapping(dest_start: Long, source_start: Long, length: Long)

  def part1(input: List[String]): Unit = {
    val seeds = input.head.split(":")(1).trim.split(" ").map(x => x.toLong).toList
    val maps = (input.tail.tail :+ "").foldLeft((List(), List()): (List[List[Mapping]], List[Mapping]))((acc, cur) => {
      if (cur.isEmpty) (acc._1 :+ acc._2, List())
      else if (cur.contains(":")) acc
      else {
        val str = cur.split(" ")
        (acc._1, Mapping(str(0).toLong, str(1).toLong, str(2).toLong) :: acc._2)
      }
    })._1

    println(maps.foldLeft(seeds: List[Long])((acc, cur) => {
      acc.map(x => {
        cur.find(y => y.source_start <= x && x < y.source_start + y.length)
          .map(y => x - y.source_start + y.dest_start)
          .getOrElse(x)
        }
      )
    }).min)
  }

  def part2(input: List[String]): Unit = {

  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day5.txt")
    val input = source.getLines().toList
    source.close()
    part1(input)
    //part2(input)
  }
}
