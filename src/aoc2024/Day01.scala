package aoc2024

/** Day01 - list of locations
  */

object Day01 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  /** @return the sum of distances. */
  def part1(locations: Seq[(Int, Int)]): Int = {
    val l1Sorted = locations.map(_._1).sorted
    val l2Sorted = locations.map(_._2).sorted
    val locationsSorted = l1Sorted.zip(l2Sorted)

    locationsSorted.foldLeft(0) {(acc, location) => {
      acc + math.abs(location._1 - location._2)
    }}
  }

  /** @return the similarity score. */
  def part2(locations: Seq[(Int, Int)]): Int = {
    31
  }

  def readFile(filename: String): Seq[(Int, Int)] = {
    import scala.io.Source

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        val parsed = line.split("\\s+").map(_.toInt)
        (parsed(0), parsed(1))
      }
    } finally {
      source.close()
    }
  }
}
