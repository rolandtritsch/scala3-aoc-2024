package aoc2024

/** Day09 - Disk Fragmenter
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
  * - we will then move around (or more specifically swap) blocks
  * - note: for that swap to work we need a split operation on a (freespace)
  *   block to make sure the blocks have the same size
  * - note: after the swap we need to check the block before/after the
  *   swapped blocks and potentially merge the blocks into one (if they
  *   are all freespace blocks)
  * - we will try to swap all files (from maxId to 1; 0 cannot be swapped
  *   because it is always the first file on the disk)
  * - then we create the checksum
  * - and then we are done
  * 
  * Note: I have tried to NOT use a mutable (double-linked) list, but was not
  * able to get it to work. The code looked even more ugly than the code
  * that uses a mutable list.
  */

object Day09p2 {
  val logger = com.typesafe.scalalogging.Logger(this.getClass.getName)

  import scala.collection.mutable

  /** A Block has an offset/address, size, id */
  type Block = (Int, Int, Option[Int])

  /** The Disk */
  class Disk (val blocks: mutable.ListBuffer[Block]) {
    override def toString(): String = {
      blocks.map { (_, blockSize, id) => id match {
        case None => '.'.toString * blockSize
        case Some(id) => id.toString * blockSize
      }}.mkString
    }
    
    /** @return true, if the file system is valid */
    def fileSystemCheck: Boolean = {
      val firstBlock = blocks.head
      blocks.tail.foldLeft(firstBlock, true) { case((pb, c), b) => {
        val (poffset, psize, pid) = pb
        val (offset, _, id) = b

        val check =if (offset != poffset + psize) {
          logger.warn(s"Offset corrupted - offset: ${offset}, poffset: ${poffset}, psize: ${psize}, id: ${id}")
          false
        } else if (id.getOrElse(-99) == pid.getOrElse(-99)) {
          logger.warn(s"Id corrupted - offset: ${offset}, poffset: ${poffset}, psize: ${psize}, id: ${id}")
          false
        } else {
          true
        }
        (b, c && check)
      }}._2
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
        // if both blocks are free space
        if (!id0.isDefined && !id1.isDefined) {
          blocks.remove(index1)
          blocks.remove(index0)
          blocks.insert(index0, (offset0, size0 + size1, id0))
        }
      }
    }

    private def foundFreeSpace(index: Int): Boolean = index >= 0

    /** defragment the file on the disk */
    def defragment(id: Int): Unit = {
      val freeIndex = findFirstFreeSpaceIndex(fileSizes(id))
      if (foundFreeSpace(freeIndex)) {
        split(freeIndex, fileSizes(id))
        // Note: We need to get the file index after the split!!!
        val fileIndex = findFileIndex(id)
        // Note: We only swap if the freeSpace is before the file!!!
        if (freeIndex < fileIndex) {
          swap(freeIndex, fileIndex)
          // Merge on the left and on the right
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
  def readFile(filename: String): Disk = {
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val blocks = mutable.ListBuffer[Block]()
    val source = Source.fromResource(filename)
    try {
      val line = source.getLines.toSeq.head
      logger.debug(s"line: ${line}")

      val firstBlockSize = line(0).toString.toInt
      blocks.insert(0, (0, firstBlockSize, Some(0)))

      val (_, _) = line.tail.grouped(2).foldLeft(firstBlockSize, 1) { case ((offset, id), chars) => {
        logger.debug(s"blocks: ${blocks}, chars: ${chars}, id: ${id}}")

        val freeBlockSize = chars(0).toString.toInt
        val freeBlocks = (offset, freeBlockSize, None)
        val usedBlockSize = chars(1).toString.toInt
        val usedBlocks = (offset + freeBlockSize, usedBlockSize, Some(id))

        blocks.insertAll(blocks.size, List(freeBlocks, usedBlocks))
        (offset + freeBlockSize + usedBlockSize, id + 1)
      }}

      Disk(blocks)
    } finally {
      source.close()
    }
  }

  /** @return the checksum for the defragmented disk */
  def part2(disk: Disk): BigInt = {
    require(disk.blocks.nonEmpty, "disk.blocks.nonEmpty")
    logger.debug(s"disk: ${disk}")

    (disk.maxId to 1 by -1).foreach(disk.defragment(_))
    //disk.fileSystemCheck
    disk.checksum
  }
}
