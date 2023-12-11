import scala.io.Source

object Day11 {
  def part1(input: List[String]): Unit = {
    val exRows = input.zip(List.range(0, input.size)).filter(x => x._1.toCharArray.forall(_ == '.')).map(_._2)
    val exCols = List.range(0, input.head.length).filter(x => input.forall(_(x) == '.'))
    val points = List.range(0, input.size).flatMap(x => List.range(0, input.head.length).map(y => (x, y)))
      .filter(p => input(p._1)(p._2) == '#')
      .map(p => (p._1 + exRows.count(_ < p._1), p._2 + exCols.count(_ < p._2)))
    val ans = points.flatMap(p1 => points.map(p2 => (p1, p2)))
      .map(ps => Math.abs(ps._1._1 - ps._2._1) + Math.abs(ps._1._2 - ps._2._2))
      .sum / 2
    println(ans)
  }

  def part2(input: List[String]): Unit = {
    val scale = 999999L
    val exRows = input.zip(List.range(0, input.size)).filter(x => x._1.toCharArray.forall(_ == '.')).map(_._2.toLong)
    val exCols = List.range(0, input.head.length).filter(x => input.forall(_(x) == '.')).map(_.toLong)
    val points = List.range(0, input.size).flatMap(x => List.range(0, input.head.length).map(y => (x.toLong, y.toLong)))
      .filter(p => input(p._1.toInt)(p._2.toInt) == '#')
      .map(p => (p._1 + scale*exRows.count(_ < p._1).toLong, p._2 + scale*exCols.count(_ < p._2).toLong))
    val ans = points.flatMap(p1 => points.map(p2 => (p1, p2)))
      .map(ps => Math.abs(ps._1._1 - ps._2._1) + Math.abs(ps._1._2 - ps._2._2))
      .sum / 2
    println(ans)
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day11.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
