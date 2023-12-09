import scala.io.Source

object Day7 {

  case class Entry1(hand: List[Long], htype: Long, bet: Long)
  def part1(input: List[String]): Unit = {
    println(input.map(x => (x.split(" ")(0).toCharArray.toList.map(y => y match {
      case '2' => 1L
      case '3' => 2L
      case '4' => 3L
      case '5' => 4L
      case '6' => 5L
      case '7' => 6L
      case '8' => 7L
      case '9' => 8L
      case 'T' => 9L
      case 'J' => 10L
      case 'Q' => 11L
      case 'K' => 12L
      case 'A' => 13L
    }), x.split(" ")(1).toLong))
      .map(x => x._1.groupBy(y => y).values.map(y => y.size).toList match {
        case a if a.contains(5) => Entry1(x._1, 7, x._2)
        case a if a.contains(4) => Entry1(x._1, 6, x._2)
        case a if a.contains(3) && a.contains(2) => Entry1(x._1, 5, x._2)
        case a if a.contains(3) => Entry1(x._1, 4, x._2)
        case a if a.size == 3 =>   Entry1(x._1, 3, x._2)
        case a if a.contains(2) => Entry1(x._1, 2, x._2)
        case _ => Entry1(x._1, 1, x._2)
      })
      .sortBy(a => a.htype * 16*16*16*16*16 + a.hand(0) * 16*16*16*16 + a.hand(1) * 16*16*16 + a.hand(2) * 16*16 + a.hand(3) * 16 + a.hand(4))
      .zip(List.range(1, input.size + 1))
      .map(x => x._1.bet * x._2)
      .sum
    )
  }

  def part2(input: List[String]): Unit = {
    println(input.map(x => (x.split(" ")(0).toCharArray.toList.map(y => y match {
      case '2' => 1L
      case '3' => 2L
      case '4' => 3L
      case '5' => 4L
      case '6' => 5L
      case '7' => 6L
      case '8' => 7L
      case '9' => 8L
      case 'T' => 9L
      case 'J' => 0L
      case 'Q' => 11L
      case 'K' => 12L
      case 'A' => 13L
    }), x.split(" ")(1).toLong))
      .map(x => ((x._1.filter(y => y != 0), x._1.count(y => y == 0), x._1), x._2))
      .map(x => (x._1._1.groupBy(y => y).values.map(y => y.size).toList, x._1._2) match {
        case (a, 0) if a.contains(5) => Entry1(x._1._3, 7, x._2)
        case (a, 1) if a.contains(4) => Entry1(x._1._3, 7, x._2)
        case (a, 0) if a.contains(4) => Entry1(x._1._3, 6, x._2)
        case (a, 0) if a.contains(3) && a.contains(2) => Entry1(x._1._3, 5, x._2)
        case (a, 2) if a.contains(3) => Entry1(x._1._3, 7, x._2)
        case (a, 1) if a.contains(3) => Entry1(x._1._3, 6, x._2)
        case (a, 0) if a.contains(3) => Entry1(x._1._3, 4, x._2)
        case (a, 1) if a.count(k => k == 2) == 2 => Entry1(x._1._3, 5, x._2)
        case (a, 0) if a.count(k => k == 2) == 2 => Entry1(x._1._3, 3, x._2)
        case (a, 3) if a.contains(2) => Entry1(x._1._3, 7, x._2)
        case (a, 2) if a.contains(2) => Entry1(x._1._3, 6, x._2)
        case (a, 1) if a.contains(2) => Entry1(x._1._3, 4, x._2)
        case (a, 0) if a.contains(2) => Entry1(x._1._3, 2, x._2)
        case (a, 5) if a.forall(k => k == 1) => Entry1(x._1._3, 7, x._2)
        case (a, 4) if a.forall(k => k == 1) => Entry1(x._1._3, 7, x._2)
        case (a, 3) if a.forall(k => k == 1) => Entry1(x._1._3, 6, x._2)
        case (a, 2) if a.forall(k => k == 1) => Entry1(x._1._3, 4, x._2)
        case (a, 1) if a.forall(k => k == 1) => Entry1(x._1._3, 2, x._2)
        case (a, 0) if a.forall(k => k == 1) => Entry1(x._1._3, 1, x._2)
        case _ => throw new IllegalStateException()
      })
      .sortBy(a => a.htype * 16 * 16 * 16 * 16 * 16 + a.hand(0) * 16 * 16 * 16 * 16 + a.hand(1) * 16 * 16 * 16 + a.hand(2) * 16 * 16 + a.hand(3) * 16 + a.hand(4))
      .zip(List.range(1, input.size + 1))
      .map(x => x._1.bet * x._2)
      .sum
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day7.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
