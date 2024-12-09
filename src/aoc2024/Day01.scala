package aoc2024

/** Day01 - Historian Hysteria
  *
  * part1:
  *
  * - some simple list processing
  * - take the two cols, put them in seperate lists, zip it,
  *   calculate the diff and sum it up
  * - done
  *
  * part2:
  *
  * - almost the same as part1
  * - but do the multiplication instead of the diff
  * - done
  */

object Day01 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  /** @return the sum of distances. */
  def part1(locations: Seq[(Int, Int)]): Int = {
    require(locations.nonEmpty, "locations.nonEmpty")
    logger.debug(s"${locations}")

    val l1Sorted = locations.map(_._1).sorted
    val l2Sorted = locations.map(_._2).sorted
    val locationsSorted = l1Sorted.zip(l2Sorted)

    locationsSorted.foldLeft(0) {(acc, location) => {
      acc + math.abs(location._1 - location._2)
    }}
  }

  /** @return the similarity score. */
  def part2(locations: Seq[(Int, Int)]): Int = {
    require(locations.nonEmpty, "locations.nonEmpty")
    logger.debug(s"${locations}")

    val l1 = locations.map(_._1)
    val l2 = locations.map(_._2)

    l1.foldLeft(0) {(acc, l) => {
      val c = l2.count(_ == l)
      acc + (l * c)
    }}
  }

  /** @return the file for the given filename as parsed elements */ 
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
