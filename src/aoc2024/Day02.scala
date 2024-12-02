package aoc2024

/** Day02 - Red-Nosed Reports
  *
  * Count all the safe reports. A report is safe, when is has a minimum
  * count of safe levels.
  */

object Day02 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Report = Seq[Int]

  private def isSafe(isIncreasing: Boolean, prevLevel: Int, level: Int) = {
    val levelDiff = math.abs(prevLevel - level)
    val levelDiffIsSafe = (1 to 3).contains(levelDiff)

    if (isIncreasing) {
      prevLevel < level && levelDiffIsSafe
    } else {
      prevLevel > level && levelDiffIsSafe
    }
  }

  private def reportIsSafe(report: Report, minAmountOfSafeLevels: (Report, Int) => Boolean): Boolean = {
    require(report.size >= 2, "report.size >= 2")
    logger.debug(s"report: ${report}")

    val (_, _, countOfSafeLevels) = report.tail.foldLeft(report(0) < report(1), report(0), 0) { (soFar, level) => {
      logger.debug(s"soFar: ${soFar}, level: ${level}")

      val (isIncreasing, prevLevel, safeCounter) = soFar

      val thisIsSafe = isSafe(isIncreasing, prevLevel, level)
      val nextLevel = if (thisIsSafe) level else prevLevel
      val nextSafeCounter = if (thisIsSafe) safeCounter + 1 else safeCounter

      (isIncreasing, nextLevel, nextSafeCounter)
    }}

    logger.debug(s"report: ${report}, countOfSafeLevels: ${countOfSafeLevels}")
    minAmountOfSafeLevels(report, countOfSafeLevels)
  }

  private def reportsAreSafe(reports: Seq[Report], minAmountOfSafeLevels: (Report, Int) => Boolean): Int = {
    logger.debug(s"reports: ${reports}")

    reports.count { (report) => {
      reportIsSafe(report, minAmountOfSafeLevels)
    }}
  }

  private def allLevels(report: Report, countOfSafeLevels: Int): Boolean = {
    countOfSafeLevels == report.size - 1
  }

  private def allLevelsButOne(report: Report, countOfSafeLevels: Int): Boolean = {
    countOfSafeLevels >= report.size - 2
  }

  /** @return the solution for part1 */
  def part1(reports: Seq[Report]): Int = {
    require(reports.nonEmpty, "reports.nonEmpty")
    logger.debug(s"reports: ${reports}")

    reportsAreSafe(reports, allLevels)
  }

  /** @return the solution for part2 */
  def part2(reports: Seq[Report]): Int = {
    require(reports.nonEmpty, "reports.nonEmpty")
    logger.debug(s"reports: ${reports}")

    reportsAreSafe(reports, allLevelsButOne)
  }

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Seq[Seq[Int]] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")
        val parsed = line.split("\\s+").map(_.toInt)
        logger.debug(s"parsed: ${parsed}")
        parsed.toIndexedSeq
      }
    } finally {
      source.close()
    }
  }
}
