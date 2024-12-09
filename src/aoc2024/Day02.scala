package aoc2024

/** Day02 - Red-Nosed Reports
  *
  * part1:
  *
  * - count all the safe reports
  * - a report is safe, when is has a minimum count of safe levels
  *
  * part2:
  *
  * - same as part1
  * - but additionally count the reports that can be made safe,
  *   by removing one element
  * - we will do this by going through the list and try isSafe
  *   on all possible (shorter by one) lists
  */

object Day02 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Report = Seq[Int]

  /** @return the reports from the given file */
  def readFile(filename: String): Seq[Report] = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"${filename}")

    val source = Source.fromFile(filename)
    try {
      source.getLines().toSeq.map { line =>
        logger.debug(s"line: ${line}")

        val parsed = line.split("\\s+").map(_.toInt)
        logger.debug(s"parsed: ${parsed}")

        parsed.toSeq
      }
    } finally {
      source.close()
    }
  }

  extension (report: Report) {
    def isSafe: Boolean = {
      require(report.size >= 2, "report.size >= 2")
      logger.debug(s"report: ${report}")

      def levelIsSafe(isIncreasing: Boolean, prevLevel: Int, level: Int) = {
        val levelDiff = math.abs(prevLevel - level)
        val levelDiffIsSafe = (1 to 3).contains(levelDiff)

        if (isIncreasing) {
          prevLevel < level && levelDiffIsSafe
        } else {
          prevLevel > level && levelDiffIsSafe
        }
      }

      val (_, _, countOfSafeLevels) =
        report.tail.foldLeft(report(0) < report(1), report(0), 0) {
          (soFar, level) =>
            {
              logger.debug(s"soFar: ${soFar}, level: ${level}")

              val (isIncreasing, prevLevel, safeCounter) = soFar

              val thisIsSafe = levelIsSafe(isIncreasing, prevLevel, level)
              val nextLevel = if (thisIsSafe) level else prevLevel
              val nextSafeCounter =
                if (thisIsSafe) safeCounter + 1 else safeCounter

              (isIncreasing, nextLevel, nextSafeCounter)
            }
        }

      logger.debug(
        s"report: ${report}, countOfSafeLevels: ${countOfSafeLevels}"
      )
      countOfSafeLevels == report.size - 1
    }

    def isSafe0: Boolean = {
      report.indices.exists { i => {
        val reportWithOneRemoved = report.take(i) ++ report.drop(i + 1)
        reportWithOneRemoved.isSafe
      }}
    }
  }

  /** @return the solution for part1 */
  def part1(reports: Seq[Report]): Int = {
    require(reports.nonEmpty, "reports.nonEmpty")
    logger.debug(s"reports: ${reports}")

    reports.count(_.isSafe)
  }

  /** @return the solution for part2 */
  def part2(reports: Seq[Report]): Int = {
    require(reports.nonEmpty, "reports.nonEmpty")
    logger.debug(s"reports: ${reports}")

    reports.count { r => {
      r.isSafe || r.isSafe0
    }}
  }
}
