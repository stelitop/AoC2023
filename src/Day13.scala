import scala.io.Source

object Day13 {
  def part1(input: List[String]): Unit = {
    println((input :+ "").foldLeft((List(), List()): (List[String], List[List[String]]))((acc, cur) => {
      if (cur.isEmpty) (List(), acc._2 :+ acc._1)
      else (acc._1 :+ cur, acc._2)
    })._2.map(s => {
        val hor = List.range(1, s.size).map(i => (i, List.range(i - Math.min(i, s.size - i), i).zip(List.range(i, i + Math.min(i, s.size - i)).reverse)))
          .filter(r => r._2.forall(p => s(p._1).equals(s(p._2))))
          .map(_._1)
          .sum*100
        val hsz = s.head.length
        val ver = List.range(1, hsz).map(i => (i, List.range(i - Math.min(i, hsz - i), i).zip(List.range(i, i + Math.min(i, hsz - i)).reverse)))
          .filter(r => r._2.forall(p => s.map(_(p._1)).zip(s.map(_(p._2))).forall(v => v._1 == v._2)))
          .map(_._1)
          .sum
        hor + ver
    }).sum)
  }

  def part2(input: List[String]): Unit = {
    println((input :+ "").foldLeft((List(), List()): (List[String], List[List[String]]))((acc, cur) => {
      if (cur.isEmpty) (List(), acc._2 :+ acc._1)
      else (acc._1 :+ cur, acc._2)
    })._2.map(s => {
      val hor = List.range(1, s.size).map(i => (i, List.range(i - Math.min(i, s.size - i), i).zip(List.range(i, i + Math.min(i, s.size - i)).reverse)))
        //.filter(r => r._2.forall(p => s(p._1).equals(s(p._2))))
        .filter(r => r._2.map(p => s(p._1).toCharArray.zip(s(p._2).toCharArray).count(v => v._1 != v._2)).sum == 1)
        .map(_._1)
        .sum * 100
      val hsz = s.head.length
      val ver = List.range(1, hsz).map(i => (i, List.range(i - Math.min(i, hsz - i), i).zip(List.range(i, i + Math.min(i, hsz - i)).reverse)))
        //.filter(r => r._2.forall(p => s.map(_(p._1)).zip(s.map(_(p._2))).forall(v => v._1 == v._2)))
        .filter(r => r._2.map(p => s.map(_(p._1)).zip(s.map(_(p._2))).count(v => v._1 != v._2)).sum == 1)
        .map(_._1)
        .sum
      hor + ver
    }).sum)
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day13.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
