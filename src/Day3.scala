import scala.io.Source

object Day3 {
  def part1(input: List[String]): Unit = {
    val locations = input
      .zip(List.range(0, input.size))
      .map(x => (x._2, x._1.toCharArray.toList.zip(List.range(0, x._1.length)).filterNot(y => ('0' <= y._1 && y._1 <= '9') || y._1 == '.')))
      .flatMap(x => x._2.map(y => (x._1, y._2)))

    val nums = input
      .zip(List.range(0, input.size))
      .map(x => (x._2, x._1.toCharArray.toList.foldLeft((List(), ("", 0)): (List[(String, Int)], (String, Int)))((acc, cur) => {
      if ('0' <= cur && cur <= '9') (acc._1, (acc._2._1 + cur, acc._2._2))
      else {
        if (acc._2._1 == "") (acc._1, (acc._2._1, acc._2._2 + 1))
        else ((acc._2._1, acc._2._2) :: acc._1, ("", acc._2._2 + 1 + acc._2._1.length))
      }
    })))
      .map(x => (x._1, if (x._2._2._1.isEmpty) x._2._1 else (x._2._2._1, x._2._2._2) :: x._2._1))
      .flatMap(x => x._2.map(y => (y._1, x._1, y._2)))

    println(nums.filter(num => locations
      .exists(pos => pos._1 - 1 <= num._2 && num._2 <= pos._1 + 1 && num._3 - 1 <= pos._2 && pos._2 <= (num._3 + num._1.length)))
      .map(_._1.toInt)
      .sum)
  }

  def part2(input: List[String]): Unit = {
    val gearLocations = input
      .zip(List.range(0, input.size))
      .map(x => (x._2, x._1.toCharArray.toList.zip(List.range(0, x._1.length)).filter(y => y._1 == '*')))
      .flatMap(x => x._2.map(y => (x._1, y._2)))

    val nums = input
      .zip(List.range(0, input.size))
      .map(x => (x._2, x._1.toCharArray.toList.foldLeft((List(), ("", 0)): (List[(String, Int)], (String, Int)))((acc, cur) => {
        if ('0' <= cur && cur <= '9') (acc._1, (acc._2._1 + cur, acc._2._2))
        else {
          if (acc._2._1 == "") (acc._1, (acc._2._1, acc._2._2 + 1))
          else ((acc._2._1, acc._2._2) :: acc._1, ("", acc._2._2 + 1 + acc._2._1.length))
        }
      })))
      .map(x => (x._1, if (x._2._2._1.isEmpty) x._2._1 else (x._2._2._1, x._2._2._2) :: x._2._1))
      .flatMap(x => x._2.map(y => (y._1, x._1, y._2)))

    println(gearLocations.map(pos =>
      nums.filter(num => pos._1 - 1 <= num._2 && num._2 <= pos._1 + 1 && num._3 - 1 <= pos._2 && pos._2 <= (num._3 + num._1.length)))
      .filter(x => x.length == 2)
      .map(x => x(0)._1.toInt * x(1)._1.toInt)
      .sum)
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day3.txt")
    val input = source.getLines().toList
    source.close()
    part1(input)
    //part2(input)
  }
}
