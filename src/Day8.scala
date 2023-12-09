import scala.io.Source

object Day8 {
  case class Input1(start: String, left: String, right: String)

  def gcd(a: Long, b: Long): Long = {
      if (a < b) gcd(b, a)
      else if (b == 0) a
      else gcd(b, a % b)
  }

  def solve1(instructions: List[Char], mapL: Map[String, String], mapR: Map[String, String], start: String, end: String): Long = {
    var ans = (start, 0L)
    println(start + " " + end)
    while (ans._1 != end) {
      ans = instructions.foldLeft((ans._1, ans._2))((acc, cur) => {
        if (acc._1 == end) acc
        else if (cur == 'L') (mapL(acc._1), acc._2 + 1)
        else (mapR(acc._1), acc._2 + 1)
      })
    }
    println(start + ": " + ans._2)
    ans._2
  }

  def solve2(instructions: List[Char], mapL: Map[String, String], mapR: Map[String, String], start: String): Long = {
    var ans = (start, 0L)
    while (!ans._1.endsWith("Z")) {
      ans = instructions.foldLeft((ans._1, ans._2))((acc, cur) => {
        if (acc._1.endsWith("Z")) acc
        else if (cur == 'L') (mapL(acc._1), acc._2 + 1)
        else (mapR(acc._1), acc._2 + 1)
      })
    }
    println(start + ": " + ans._2)
    ans._2
  }

  def part1(input: List[String]): Unit = {
    val instructions = input.head.toList
    val maps = input.tail.tail
      .map(x => Input1(x.substring(0, 3), x.substring(7, 10), x.substring(12, 15)))
      .foldLeft((Map(), Map()): (Map[String, String], Map[String, String]))((acc, cur) => {
        (acc._1 + (cur.start -> cur.left), acc._2 + (cur.start -> cur.right))
      })

    println(solve1(instructions, maps._1, maps._2, "AAA", "ZZZ"))
  }

  def part2(input: List[String]): Unit = {
    val instructions = input.head.toList
    val maps = input.tail.tail
      .map(x => Input1(x.substring(0, 3), x.substring(7, 10), x.substring(12, 15)))
      .foldLeft((Map(), Map()): (Map[String, String], Map[String, String]))((acc, cur) => {
        (acc._1 + (cur.start -> cur.left), acc._2 + (cur.start -> cur.right))
      })

    val starts = maps._1.keys.filter(x => x.endsWith("A")).toList
    val cycles = starts.map(x => solve2(instructions, maps._1, maps._2, x))
    println(cycles.foldLeft(1L)((acc, cur) => {acc*cur / gcd(acc, cur)}))
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day8.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
