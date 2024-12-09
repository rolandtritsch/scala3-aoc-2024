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
  * - this is driven by the file id
  * - we move files by decreasing file id number
  * - we first need to look up the size of the file
  * - we then need to find the first freespace that can fit the file
  *   - note: that freespace needs to be left of the file
  * - we then construct a new disk with the new block layout and try again
  * - until we have tried to move all files
  */

object Day09 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  type Block = (Int, Option[Int])

  /** The disk */
  class Disk (val blocks: Vector[Block], val fileToBeDefragmented: Int) {
    def this(blocks: Vector[Block]) = {
      this(blocks, blocks.map(_._2).flatten.max)
    }

    override def toString(): String = {
      blocks.map { b => b match {
        case (_, None) => '.'
        case (_, Some(id)) => id.toString.head
      }}.mkString
    }

    val usedBlocks = blocks.map(_._2).flatten
    val fileSizes = usedBlocks.groupBy(identity).view.mapValues(_.size).toMap

    def defragment: Disk = {
      def defragmentor(fragmentedBlocks: Vector[Block], defragmentedBlocks: Vector[Block], blocksAvailableForDefragmentation: Vector[Int], n: Int): Vector[Block] = {
        logger.debug(s"fragmentedBlocks: ${fragmentedBlocks}, defragmentedBlocks: ${defragmentedBlocks}, blocksAvailableForDefragmentation: ${blocksAvailableForDefragmentation}, n: ${n}")

        if(n >= usedBlocks.size) {
          defragmentedBlocks ++ Vector.fill(blocks.size - usedBlocks.size)((-99, None))
        } else fragmentedBlocks match {
          case Vector((_, None), _*) => defragmentor(fragmentedBlocks.tail, defragmentedBlocks :+ (-99, Some(blocksAvailableForDefragmentation.head)), blocksAvailableForDefragmentation.tail, n + 1)
          case Vector((_, Some(id)), _*) =>  defragmentor(fragmentedBlocks.tail, defragmentedBlocks :+ (-99, Some(id)), blocksAvailableForDefragmentation, n + 1)
          case _ => throw new RuntimeException("Unexpected case")
        }
      }

      Disk(defragmentor(blocks, Vector(), usedBlocks.reverse, 0))
    }

    def buildNewBlocks(spaceFoundIndex: Int, fileToBeDefragmentedSize: Int): Vector[Block] = {
      val (blockSize, _) = blocks(spaceFoundIndex)
      val newBlockHead = blocks.take(spaceFoundIndex)
      val newBlockMoved = Vector.fill(fileToBeDefragmentedSize)(fileToBeDefragmentedSize, Some(fileToBeDefragmented))
      val newBlockStillFree = Vector.fill(blockSize - fileToBeDefragmentedSize)(blockSize - fileToBeDefragmentedSize, None)
      val newBlockTail = blocks.drop(spaceFoundIndex + blockSize).map { b => b match {
        case (_, Some(id)) if(id == fileToBeDefragmented) => (-99, None)
        case b => b
      }}
      newBlockHead ++ newBlockMoved ++ newBlockStillFree ++ newBlockTail
    }

    def defragment0: Disk = {
      val fileToBeDefragmentedSize = fileSizes(fileToBeDefragmented)
      val spaceFoundIndex = blocks.indexWhere { b => b match {
        case (blockSize, None) if(blockSize >= fileToBeDefragmentedSize) => true
        case _ => false
      }}

      if (spaceFoundIndex >= 0) {
        val newBlocks = buildNewBlocks(spaceFoundIndex, fileToBeDefragmentedSize)
        Disk(newBlocks, fileToBeDefragmented - 1)
      } else {
        Disk(blocks, fileToBeDefragmented - 1)
      }
    }

    def checksum: BigInt = {
      usedBlocks.map(BigInt(_)).zipWithIndex.map(_ * _).sum
    }

    def checksum0: BigInt = {
      blocks.map { (_, b) => b match {
        case Some(id) => BigInt(id)
        case None => BigInt(0)
      }}.zipWithIndex.map(_ * _).sum
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
      val firstBlock: Vector[Block] = Vector.fill(firstBlockSize)((firstBlockSize, Some(0)))
      val blocks = line.tail.grouped(2).foldLeft(firstBlock, 1) { case ((blocks, id), chars) => {
        logger.debug(s"blocks: ${blocks}, chars: ${chars}, id: ${id}}")
        val freeBlockSize = chars(0).toString.toInt
        val freeBlocks: Vector[Block] = Vector.fill(freeBlockSize)((freeBlockSize, None))
        val usedBlockSize = chars(1).toString.toInt
        val usedBlocks: Vector[Block] = Vector.fill(usedBlockSize)((usedBlockSize, Some(id)))
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

    val defragmentedDisk = (disk.fileToBeDefragmented to 0).foldLeft(disk) { (d, n) => {
      val defragmentedBlocks = d.defragment0.blocks
      Disk(defragmentedBlocks, n - 1)
    }}
    defragmentedDisk.checksum0
  }
}
