import scala.io.Source

object Day12 {

  def verify(in: Input): Boolean = {
    in.data.split("\\.").toList.filter(x => x.nonEmpty).map(_.length).equals(in.parts)
  }
  case class Input(data: String, parts: List[Int])
  def part1(input: List[String]): Unit = {
    println(input.map(x => Input(x.split(" ")(0), x.split(" ")(1).split(",").toList.map(_.toInt)))
      .map(x => {
        def help(str: String, cum: String, idx: Int): Int = {
          if (idx == str.length) {
            if (verify(Input(cum, x.parts))) 1
            else 0
          } else {
            if (str(idx) == '?') help(str, cum + ".", idx + 1) + help(str, cum + '#', idx + 1);
            else help (str, cum + str(idx), idx + 1)
          }
        }
        help(x.data, "", 0)
      })
      .sum
    )
  }

  def solveSub(s: String, parts: List[Int]) : Long =  {
    if (parts.isEmpty) {
      if (s.contains('#')) return 0
      else return 1
    }
    if (s.length < parts.sum + parts.size - 1) return 0
    val firstMand = s.indexOf('#')
    if (firstMand == -1) {
      var mem: scala.collection.mutable.Map[(Int, Int), Long] = scala.collection.mutable.Map()
      def solveEz(k: Int, n: Int): Long = {
        if (n == 1) return 1
        if (k == 0) return 1
        if (mem.contains((k, n))) return mem((k, n))
        mem.put((k, n), List.range(0, k+1).map(i => solveEz(k - i, n - 1)).sum)
        return mem((k, n))
      }
      return solveEz(s.length - parts.sum - parts.length + 1, parts.size + 1)
    }
    List.range(0, Math.min(s.length - parts.head + 1, firstMand + 1))
      .filter(i => (parts.head + i == s.length) || (s(parts.head + i) != '#'))
      .map(i => {solveSub({if (i + parts.head + 1 < s.length) s.substring(i + parts.head + 1) else ""}, parts.tail)})
      .sum
  }

  def part2(input: List[String]): Unit = {
    //"???#???????????? 3,5"
    //println(solveSub("???#????????????????#????????????????#????????????????#????????????????#????????????", List(3, 5, 3, 5, 3, 5, 3, 5, 3, 5)))
    //println(solveSub("???????????????????????????????????????????????????????????????????????????????", List(3, 5, 3, 5, 3, 5, 3, 5, 3, 5)))
    //println(solveSub("??????", List(1, 1)))

    //println(solveSub("###", List(3)))
    //println()

    input.map(x => Input(x.split(" ")(0), x.split(" ")(1).split(",").toList.map(_.toInt)))
      .map(x => Input(x.data + "?" + x.data + "?" + x.data + "?" + x.data + "?" + x.data, x.parts ::: x.parts ::: x.parts ::: x.parts ::: x.parts))
      .map(x => (x.data.split("\\.").filter(_.nonEmpty), x.parts))
      .map(x => {
        println("start")
        println(x._1.toList)
        println(x._2)
        var mem: scala.collection.mutable.Map[(Int, Int), Long] = scala.collection.mutable.Map()
        def dpsol(n: Int, k: Int): Long = {
          //println("Called n=" + n + ", k=" + k)
          if (n == 0 && k == 0) return 1
          if (n == 0) return 0
          if (k < n) return 0
          if (mem.contains((n, k))) return mem((n, k))
          val ans = List.range(1, k + 1).map(i => {
            val sub = solveSub(x._1(x._1.length - n), x._2.slice(x._2.size - k, x._2.size - k + i))
            val dp = dpsol(n - 1, k - i)
            //println("Insert segments [" + (x._2.size - k) + ", " + (x._2.size - k + i - 1) + "] into section " + (x._1.length - n))
            //println(x._2.slice(x._2.size - k, x._2.size - k + i))
            //println("Sub = " + sub + ", dp = " + dp)
            sub * dp
          }).sum
          mem.put((n, k), ans)
          //println("Got " + ans)
          return ans
        }
        val ans = dpsol(x._1.length, x._2.size)
        println(ans)
        ans
      })
  }

  def part2_2 (input: List[String]): Unit = {
    println(input.map(x => Input(x.split(" ")(0), x.split(" ")(1).split(",").toList.map(_.toInt)))
      .map(x => Input(x.data + "?" + x.data + "?" + x.data + "?" + x.data + "?" + x.data, x.parts ::: x.parts ::: x.parts ::: x.parts ::: x.parts))
      .map(x => {
        var mem: scala.collection.mutable.Map[(Int, Int), Long] = scala.collection.mutable.Map()

        for (n <- List.range(0, x.data.length)) {
          for (k <- List.range(0, x.parts.size)) {
            var cum = 0L
            if (n != 0 && x.data(n) != '#') cum += mem((n - 1, k))
            if (n+1 >= x.parts(k) && x.data.substring(n - x.parts(k) + 1, n + 1).forall(c => c == '#' || c == '?')) {
              if ((n < x.parts(k) || x.data(n - x.parts(k)) != '#') && (n == x.data.length - 1 || x.data(n+1) != '#')) {
                if (k == 0) {
                  if (x.data.substring(0, Math.max(n - x.parts(k), 0)).forall(_ != '#')) cum += 1L
                }
                else cum += mem.getOrElse((n - x.parts(k) - 1, k - 1), 0L)
              }
            }
            mem.put((n, k), cum)
          }
        }
//
//        for (k <- List.range(0, x.parts.size)) {
//          for (n <- List.range(0, x.data.length)) {
//            print(mem((n, k)) + " ")
//          }
//          println()
//        }

        mem((x.data.length - 1, x.parts.size - 1))
      }).sum
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day12.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2_2(input)
  }
}
