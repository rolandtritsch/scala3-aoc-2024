package aoc2024

/** Day09 - Disk Fragmenter
  *
  * part1:
  *
  * - lets start simple
  * - lets create a Vector[Option[Int]] to represent the Disk
  * - lets iterate through the blocks and fill the freespace
  *   with blocks from the back (until we have filled all
  *   the freespace)
  * - note: I was struggling to understand when to stop filling
  *   freespace. But then I realized that I can stop, when I have
  *   filled the first N blocks with N being the number of used
  *   blocks
  * - then we just need to do the checksum
  * - done
  *
  * part2:
  *
  * Hhhmmm ... much more difficult (at least for me). Let's see ...
  *
  * - the part1 approach will not work (believe me: I have tried!)
  * - this is a restart ...
  * - we need to model this with blocks that have a size and Some(value)
  * - note: I will probably also add the index/address of the block (to
  *   allow for the implementation of a filesystem check)
  * - we will then move around or more specifically swap blocks
  * - note: for that swap to work we need a split operation on a (freespace)
  *   block to make sure the blocks have the same size
  * - note: after the swap we need to check the block before/after the
  *   swapped blocks and potentially merge the blocks into one (if they
  *   are all freespace blocks)
  * - we will try to swap all files (from maxId to 1; 0 cannot be swapped
  *   because it is always the first file on the disk)
  * - then we create the checksum
  * - and then we are done
  */

object Day09 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Block = Option[Int]

  /** The disk */
  class Disk (val blocks: Vector[Block]) {
    val usedBlocks = blocks.flatten

    override def toString(): String = {
      blocks.map { b => b match {
        case None => '.'
        case Some(id) => id.toString.head
      }}.mkString
    }

    def defragment: Disk = {
      def defragmentor(fragmentedBlocks: Vector[Block], defragmentedBlocks: Vector[Block], blocksAvailableForDefragmentation: Vector[Int], n: Int): Vector[Block] = {
        logger.debug(s"fragmentedBlocks: ${fragmentedBlocks}, defragmentedBlocks: ${defragmentedBlocks}, blocksAvailableForDefragmentation: ${blocksAvailableForDefragmentation}, n: ${n}")

        if(n >= usedBlocks.size) {
          defragmentedBlocks ++ Vector.fill(blocks.size - usedBlocks.size)(None)
        } else fragmentedBlocks match {
          case Vector(None, _*) => defragmentor(fragmentedBlocks.tail, defragmentedBlocks :+ Some(blocksAvailableForDefragmentation.head), blocksAvailableForDefragmentation.tail, n + 1)
          case Vector(Some(id), _*) =>  defragmentor(fragmentedBlocks.tail, defragmentedBlocks :+ Some(id), blocksAvailableForDefragmentation, n + 1)
          case _ => throw new RuntimeException("Unexpected case")
        }
      }

      Disk(defragmentor(blocks, Vector(), usedBlocks.reverse, 0))
    }

    def checksum: BigInt = {
      usedBlocks.map(BigInt(_)).zipWithIndex.map(_ * _).sum
    }
  }


  /** @return the file for the given filename as parsed elements */ 
  def readFile(filename: String): Disk = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromFile(filename)
    try {
      val line = source.getLines.toSeq.head
      logger.debug(s"line: ${line}")
      val firstBlockSize = line(0).toString.toInt
      val firstBlock: Vector[Block] = Vector.fill(firstBlockSize)(Some(0))
      val blocks = line.tail.grouped(2).foldLeft(firstBlock, 1) { case ((blocks, id), chars) => {
        logger.debug(s"blocks: ${blocks}, chars: ${chars}, id: ${id}}")
        val freeBlockSize = chars(0).toString.toInt
        val freeBlocks: Vector[Block] = Vector.fill(freeBlockSize)(None)
        val usedBlockSize = chars(1).toString.toInt
        val usedBlocks: Vector[Block] = Vector.fill(usedBlockSize)(Some(id))
        (blocks ++ freeBlocks ++ usedBlocks, id + 1)
      }}
      Disk(blocks._1)
    } finally {
      source.close()
    }
  }

  /** @return the checksum for the defragmented disk */
  def part1(disk: Disk): BigInt = {
    require(disk.blocks.nonEmpty, "disk.blocks.nonEmpty")
    logger.debug(s"disk: ${disk}")

    disk.defragment.checksum
  }

  /** @return the solution for part2 */
  def part2(disk: Disk): BigInt = {
    require(disk.blocks.nonEmpty, "disk.blocks.nonEmpty")
    logger.debug(s"disk: ${disk}")

    BigInt(0)
  }
}
