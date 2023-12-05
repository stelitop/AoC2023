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

  case class Range(start: Long, length: Long)

  def part2(input: List[String]): Unit = {
    val seeds = input.head.split(":")(1).trim.split(" ").map(x => x.toLong).toList.grouped(2).map(x => Range(x(0), x(1))).toList
    val maps = (input.tail.tail :+ "").foldLeft((List(), List()): (List[List[Mapping]], List[Mapping]))((acc, cur) => {
      if (cur.isEmpty) (acc._1 :+ acc._2, List())
      else if (cur.contains(":")) acc
      else {
        val str = cur.split(" ")
        (acc._1, Mapping(str(0).toLong, str(1).toLong, str(2).toLong) :: acc._2)
      }
    })._1

    println(maps.foldLeft(seeds: List[Range])((curSeeds, curMaps) => {
        //println(seeds.map(x => x.length).sum)
      //println("New round")
        curMaps.foldLeft(curSeeds: List[Range])((curRanges, newMap) => {
          //println(curRanges)
          curRanges.flatMap(range => {
            if (range.start + range.length <= newMap.source_start) List(range)
            else if (newMap.source_start + newMap.length <= range.start) List(range)
            else if (range.start < newMap.source_start) {
              if (range.start + range.length <= newMap.source_start + newMap.length) {
                //println(1)
                List(Range(range.start, newMap.source_start - range.start), Range(newMap.dest_start, range.start + range.length - newMap.source_start))
              } else {
                //println(2)
                List(Range(range.start, newMap.source_start - range.start), Range(newMap.dest_start, newMap.length), Range(newMap.source_start + newMap.length, range.start + range.length - newMap.source_start - newMap.length))
              }
            } else {
              if (range.start + range.length <= newMap.source_start + newMap.length) {
                //println(3)
                List(Range(newMap.dest_start + range.start - newMap.source_start, range.length))
              } else {
                //println(4)
                List(Range(newMap.dest_start + range.start - newMap.source_start, newMap.source_start + newMap.length - range.start), Range(newMap.source_start + newMap.length, range.start + range.length - newMap.source_start - newMap.length))
              }
            }
          })
        })
      })
      .filter(x => x.length != 0)
      .map(x => x.start).min
    )
  }

  def main(args: Array[String]): Unit = {
    val source = Source
      .fromFile("input/Day5.txt")
    val input = source.getLines().toList
    source.close()
    //part1(input)
    part2(input)
  }
}
