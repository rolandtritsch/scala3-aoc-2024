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

  import scala.collection.mutable

  type Block2 = (Int, Int, Option[Int])

  /** The disk */
  class Disk2 (val blocks: mutable.ListBuffer[Block2]) {
    override def toString(): String = {
      blocks.map { (_, blockSize, id) => id match {
        case None => '.'.toString * blockSize
        case Some(id) => id.toString * blockSize
      }}.mkString
    }

    def fileSystemCheck: Boolean = {
      val firstBlock = blocks.head
      blocks.tail.foldLeft(firstBlock) { case(pb, b) => {
        logger.debug(s"pb: ${pb}, b: ${b}")

        val (poffset, psize, pid) = pb
        val (offset, _, id) = b

        assert(poffset + psize == offset)
        assert(pid.getOrElse(-99) != id.getOrElse(-99))
        b
      }}
      true
    }

    /** map to look up the size of the file by file id */
    val fileSizes = blocks.filter(_._3.isDefined).map((_, size, id) => (id.get, size)).toMap

    /** @return the index of the file */
    def findFileIndex(id0: Int): Int = {
      val index = blocks.indexWhere { (_, _, id1) => id1 match {
        case Some(id2) if(id2 == id0) => true
        case _ => false
      }}
      assert(index >= 0, "file id not found")
      index
    }

    /** @return the index of the first free space of minSize size */
    def findFirstFreeSpaceIndex(minSize: Int): Int = {
      blocks.indexWhere { (_, size, id) => id match {
        case None if(size >= minSize) => true
        case _ => false
      }}
    }

    /** update the block list with the free space rightsized to be swapped */
    def split(freeIndex: Int, minSize: Int): Unit = {
      val block = blocks(freeIndex)
      val (offset, size, id) = block
      val splitBlocks =
        if(size > minSize) List((offset, minSize, id), (offset + minSize, size - minSize, id))
        else List(block)

      blocks.remove(freeIndex)
      blocks.insertAll(freeIndex, splitBlocks)
    }

    /** update the block list with the swapped blocks */
    def swap(freeIndex: Int, fileIndex: Int): Unit = {
      val (fileOffset, fileSize, fileId) = blocks.remove(fileIndex)
      val (freeOffset, freeSize, freeId) = blocks.remove(freeIndex)
  
      blocks.insert(freeIndex, (freeOffset, fileSize, fileId))
      blocks.insert(fileIndex, (fileOffset, freeSize, freeId))
      }

    /** merge the two block, if they are both free space */
    def merge(index0: Int, index1: Int): Unit = {
      if (index0 >= 0 && index1 < blocks.size) {
        val (offset0, size0, id0) = blocks(index0)
        val (offset1, size1, id1) = blocks(index1)
        if (!id0.isDefined && !id1.isDefined) {
          blocks.remove(index1)
          blocks.remove(index0)
          blocks.insert(index0, (offset0, size0 + size1, id0))
        }
      }
    }

    /** defragment the file on the disk */
    def defragment(id: Int): Unit = {
      val freeIndex = findFirstFreeSpaceIndex(fileSizes(id))
      if (freeIndex >= 0) {
        split(freeIndex, fileSizes(id))
        val fileIndex = findFileIndex(id)
        if (freeIndex < fileIndex) {
          swap(freeIndex, fileIndex)
          merge(fileIndex, fileIndex + 1)
          merge(fileIndex - 1, fileIndex)
        }
      }
    }

    def maxId: Int = {
      val (_, _, Some(id)) = blocks.last: @unchecked
      id
    }

    def checksum: BigInt = {
      val ids = blocks.flatMap((_, size, id) => List.fill(size)(BigInt(id.getOrElse(0))))
      ids.zipWithIndex.map(_ * _).sum
    }
  }

  /** @return the file for the given filename as parsed elements */ 
  def readFile2(filename: String): Disk2 = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val emptyBlocks = mutable.ListBuffer[Block2]()
    val source = Source.fromFile(filename)
    try {
      val line = source.getLines.toSeq.head
      logger.debug(s"line: ${line}")

      val firstBlockSize = line(0).toString.toInt
      val firstBlock = emptyBlocks :+ (0, firstBlockSize, Some(0))

      val (blocks, _, _) = line.tail.grouped(2).foldLeft(firstBlock, firstBlockSize, 1) { case ((bs, offset, id), chars) => {
        logger.debug(s"bs: ${bs}, chars: ${chars}, id: ${id}}")

        val freeBlockSize = chars(0).toString.toInt
        val freeBlocks = (offset, freeBlockSize, None)
        val usedBlockSize = chars(1).toString.toInt
        val usedBlocks = (offset + freeBlockSize, usedBlockSize, Some(id))

        (bs :+ freeBlocks :+ usedBlocks, offset + freeBlockSize + usedBlockSize, id + 1)
      }}

      Disk2(blocks)
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

  /** @return the checksum for the defragmented disk */
  def part2(disk: Disk2): BigInt = {
    require(disk.blocks.nonEmpty, "disk.blocks.nonEmpty")
    logger.debug(s"disk: ${disk}")

    (disk.maxId to 1 by -1).foreach(disk.defragment(_))
    //disk.fileSystemCheck
    disk.checksum
  }
}
