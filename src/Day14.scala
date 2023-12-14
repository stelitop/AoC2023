import scala.io.Source

object Day14 {
  def part1(input: List[String]): Unit = {
    val data = List.range(0, input.size).flatMap(x => List.range(0, input.head.length).map(y => (x, y)))
      .foldLeft((List(), Set()): (List[(Int, Int)], Set[(Int, Int)]))((acc, cur) => input(cur._1)(cur._2) match {
        case '#' => (acc._1, acc._2 + cur)
        case 'O' => (acc._1 :+ cur, acc._2)
        case _ => acc
      })
    println(data._1.foldLeft((0, data._2): (Int, Set[(Int, Int)]))((acc, cur) => {
      def helper(p: (Int, Int), score: Int, walls: Set[(Int, Int)]): (Int, Set[(Int, Int)]) = {
        if (p._1 == 0 || walls.contains((p._1 - 1, p._2))) (score + input.size - p._1, walls + p)
        else helper((p._1 - 1, p._2), score, walls)
      }
      helper(cur, acc._1, acc._2)
    })._1)
  }

  def moveDirection(direction: Int, rocks: List[(Int, Int)], walls: Set[(Int, Int)]): List[(Int, Int)] = {
    val offset = direction match {
      case 0 => (-1, 0)
      case 1 => (0, 1)
      case 2 => (1, 0)
      case 3 => (0, -1)
    }
    rocks.sortWith((a, b) => direction match {
      case 0 => a._1 < b._1
      case 1 => a._2 > b._2
      case 2 => a._1 > b._1
      case 3 => a._2 < b._2
    }).foldLeft((List(), walls): (List[(Int, Int)], Set[(Int, Int)]))((acc, cur) => {
      def helper(p: (Int, Int), walls: Set[(Int, Int)]): (Int, Int) = {
        if (walls.contains((p._1 + offset._1, p._2 + offset._2))) p
        else helper((p._1 + offset._1, p._2 + offset._2), walls)
      }
      val p = helper(cur, acc._2)
      (p :: acc._1, acc._2 + p)
    })._1
  }

  def cycle(rocks: List[(Int, Int)], walls: Set[(Int, Int)]): List[(Int, Int)] = {
    val int1 = moveDirection(0, rocks, walls)
    val int2 = moveDirection(3, int1, walls)
    val int3 = moveDirection(2, int2, walls)
    val int4 = moveDirection(1, int3, walls)
    int4
  }

  def ezSort(rocks: List[(Int, Int)]): List[(Int, Int)] = {
    rocks.sortBy(a => a._1*10000 + a._2)
  }

  def part2(input: List[String]): Unit = {
    val data = List.range(0, input.size).flatMap(x => List.range(0, input.head.length).map(y => (x, y)))
      .foldLeft((List(), Set()): (List[(Int, Int)], Set[(Int, Int)]))((acc, cur) => input(cur._1)(cur._2) match {
        case '#' => (acc._1, acc._2 + cur)
        case 'O' => (acc._1 :+ cur, acc._2)
        case _ => acc
      })
    var rocks = data._1
    val walls = List.range(-1, input.size + 1).flatMap(i => List((i, -1), (i, input.size), (-1, i), (input.size, i)))
      .foldLeft(data._2)((acc, cur) => acc + cur)

    val seen: scala.collection.mutable.Map[List[(Int, Int)], Int] = scala.collection.mutable.Map()
    seen.put(ezSort(rocks), 0)
    var limit = 1000000000
    for (i <- List.range(1, 1000)) {
      rocks = ezSort(cycle(rocks, walls))
      if (seen.contains(rocks)) {
        val loopSize: Int = i - seen(rocks)
        for (j <- List.range(((limit - i)/loopSize)*loopSize + i + 1, limit + 1)) {
          rocks = cycle(rocks, walls)
        }
        println(rocks.map(input.size - _._1).sum)
        return
      }
      else {
        seen.put(rocks, i)
      }
    }
    println("1000 ayo")
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day14.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
