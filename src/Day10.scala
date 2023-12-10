import scala.io.Source

object Day10 {
  def part1(input: List[String]): Unit = {
    val mapping = input
      .map(x => x.toCharArray.zip(List.range(0, x.length)))
      .zip(List.range(0, input.size))
      .flatMap(x => x._1.map(y => ((x._2, y._2), y._1)))
      .foldLeft(Map(): Map[(Int, Int), Char])((acc, cur) => acc + (cur._1 -> cur._2))
    // start should be an F
    val startPos = mapping.find(x => x._2 == 'S').get._1
    def trav(pos: (Int, Int), explored: Set[(Int, Int)]): Int = mapping(pos) match {
      case 'L' | '|' | 'J' if !explored.contains((pos._1 - 1, pos._2)) => 1 + trav((pos._1 - 1, pos._2), explored + ((pos._1 - 1, pos._2)))
      case '7' | '|' | 'F' if !explored.contains((pos._1 + 1, pos._2)) => 1 + trav((pos._1 + 1, pos._2), explored + ((pos._1 + 1, pos._2)))
      case '7' | '-' | 'J' if !explored.contains((pos._1, pos._2 - 1)) => 1 + trav((pos._1, pos._2 - 1), explored + ((pos._1, pos._2 - 1)))
      case 'L' | '-' | 'F' if !explored.contains((pos._1, pos._2 + 1)) => 1 + trav((pos._1, pos._2 + 1), explored + ((pos._1, pos._2 + 1)))
      case _ => 1
    }
    val ans = trav((startPos._1 + 1, startPos._2), Set(startPos, (startPos._1 + 1, startPos._2))) + 1
    println(ans/2)
  }

  def part2(input: List[String]): Unit = {
    val mapping = input
      .map(x => x.toCharArray.zip(List.range(0, x.length)))
      .zip(List.range(0, input.size))
      .flatMap(x => x._1.map(y => ((x._2, y._2), y._1)))
      .foldLeft(Map(): Map[(Int, Int), Char])((acc, cur) => acc + (cur._1 -> cur._2))
    // start should be an F
    val startPos = mapping.find(x => x._2 == 'S').get._1

    val doubledMapping = mapping.map(x => ((x._1._1 * 2, x._1._2 * 2), x._2)).foldLeft(Map(): Map[(Int, Int), Char])((acc, cur) => cur._2 match {
      case 'L' => ((acc + ((cur._1._1 - 1, cur._1._2) -> '|')) + ((cur._1._1, cur._1._2 + 1) -> '-')) + cur
      case 'J' => ((acc + ((cur._1._1 - 1, cur._1._2) -> '|')) + ((cur._1._1, cur._1._2 - 1) -> '-')) + cur
      case 'F' | 'S' => ((acc + ((cur._1._1 + 1, cur._1._2) -> '|')) + ((cur._1._1, cur._1._2 + 1) -> '-')) + cur
      case '7' => ((acc + ((cur._1._1 + 1, cur._1._2) -> '|')) + ((cur._1._1, cur._1._2 - 1) -> '-')) + cur
      case '-' => ((acc + ((cur._1._1, cur._1._2 - 1) -> '-')) + ((cur._1._1, cur._1._2 + 1) -> '-')) + cur
      case '|' => ((acc + ((cur._1._1 - 1, cur._1._2) -> '|')) + ((cur._1._1 + 1, cur._1._2) -> '|')) + cur
      case _ => acc
    })

    def trav(pos: (Int, Int), explored: Set[(Int, Int)]): Set[(Int, Int)] = doubledMapping(pos) match {
      case 'L' | '|' | 'J' if !explored.contains((pos._1 - 1, pos._2)) => trav((pos._1 - 1, pos._2), explored + ((pos._1 - 1, pos._2)))
      case '7' | '|' | 'F' if !explored.contains((pos._1 + 1, pos._2)) => trav((pos._1 + 1, pos._2), explored + ((pos._1 + 1, pos._2)))
      case '7' | '-' | 'J' if !explored.contains((pos._1, pos._2 - 1)) => trav((pos._1, pos._2 - 1), explored + ((pos._1, pos._2 - 1)))
      case 'L' | '-' | 'F' if !explored.contains((pos._1, pos._2 + 1)) => trav((pos._1, pos._2 + 1), explored + ((pos._1, pos._2 + 1)))
      case _ => explored
    }
    val loop = trav((startPos._1*2 + 1, startPos._2*2), Set((startPos._1 * 2, startPos._2 * 2), (startPos._1*2 + 1, startPos._2*2)))
    val extendedMap = List.range(-1, input.size*2).flatMap(x => List.range(-1, input.head.length*2).map(y => (x, y))).foldLeft(Set(): Set[(Int, Int)])((acc, cur) => acc + cur)

    def bfs(q: List[(Int, Int)], explored: Set[(Int, Int)]): Set[(Int, Int)] = q match {
      case Nil => explored
      case cur :: tail => {
          val added = List((cur._1 + 1, cur._2), (cur._1 - 1, cur._2), (cur._1, cur._2 + 1), (cur._1, cur._2 - 1))
          .filter(extendedMap.contains).filter(!explored.contains(_)).filter(!loop.contains(_))
          bfs(tail ::: added, added.foldLeft(explored)((acc, cur) => acc + cur))
        }
      }
    val covered = bfs(List((-1, -1)), Set((-1, -1)))
    val ans = extendedMap.count(x => x._1 % 2 == 0 && x._2 % 2 == 0) - covered.count(x => x._1 % 2 == 0 && x._2 % 2 == 0) - loop.count(x => x._1 % 2 == 0 && x._2 % 2 == 0)
    println(ans)
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day10.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
