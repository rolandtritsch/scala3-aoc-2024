package aoc2024

/** Day02 - template
  */

object Day02 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  private def levelIsSafe(isIncreasing: Boolean, prevLevel: Int, level: Int) = {
    val levelDiff = math.abs(prevLevel - level)
    if (isIncreasing) {
      prevLevel < level && levelDiff >= 1 && levelDiff <= 3
    } else {
      prevLevel > level && levelDiff >= 1 && levelDiff <= 3
    }
  }

  private def reportIsSafe(report: Seq[Int], dampen: Boolean): Boolean = {
    val initialIsIncreasing = report(0) < report(1)
    val levelDiff = math.abs(report(0) - report(1))
    val inititialIsSafe = levelDiff >= 1 && levelDiff <= 3

    val (safe, _, _, count) = report.tail
      .foldLeft(inititialIsSafe, initialIsIncreasing, report(0), 0) {
        (soFar, level) =>
          {
            logger.debug(s"${soFar}")
            val (safe, isIncreasing, prevLevel, counter) = soFar
            val thisIsSave = levelIsSafe(isIncreasing, prevLevel, level)
            val thisCounter = if (thisIsSave) counter + 1 else counter
            (safe && thisIsSave, isIncreasing, level, thisCounter)
          }
      }

    if (dampen) count >= report.size - 2 else count == report.size - 1
  }

  private def reportsAreSafe(reports: Seq[Seq[Int]], dampen: Boolean): Int = {
    reports.count { (report) =>
      {
        logger.debug(s"${report}")
        reportIsSafe(report, dampen)
      }
    }
  }

  /** @return the solution for part1 */
  def part1(reports: Seq[Seq[Int]]): Int = {
    require(reports.nonEmpty, "reports.nonEmpty")
    logger.debug(s"${reports}")

    reportsAreSafe(reports, false)
  }

  /** @return the solution for part2 */
  def part2(reports: Seq[Seq[Int]]): Int = {
    require(reports.nonEmpty, "reports.nonEmpty")
    logger.debug(s"${reports}")

    reportsAreSafe(reports, true)
  }

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Seq[Seq[Int]] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"${line}")
        val parsed = line.split("\\s+").map(_.toInt)
        logger.debug(s"${parsed}")
        parsed
      }
    } finally {
      source.close()
    }
  }
}
