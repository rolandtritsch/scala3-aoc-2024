package aoc2024

class Day09Test extends munit.ScalaCheckSuite:
    val only = new munit.Tag("only")
    val ignore = new munit.Tag("ignore")

    test("Day09 - dummy".tag(ignore)):
        assert(true)

    test("Day09p1 - readFile - test"):
        val obtained = Day09p1.readFile("./inputs/Day09Test.txt")
        assertEquals(obtained.blocks.size, 42)
        assertEquals(
            obtained.toString,
            "00...111...2...333.44.5555.6666.777.888899"
        )

    test("Day09p1 - readFile"):
        val obtained = Day09p1.readFile("./inputs/Day09.txt")
        assertEquals(obtained.blocks.size, 95482)

    test("Day09p1 - defragment"):
        val disk = Day09p1.readFile("./inputs/Day09Test.txt")
        val obtained = disk.defragment
        val expected = "0099811188827773336446555566.............."

        assertEquals(obtained.toString, expected)

    test("Day09p1 - part1 - test"):
        val input = Day09p1.readFile("./inputs/Day09Test.txt")
        val obtained = Day09p1.part1(input)
        assertEquals(obtained, BigInt("1928"))

    test("Day09p1 - part1"):
        val input = Day09p1.readFile("./inputs/Day09.txt")
        val obtained = Day09p1.part1(input)
        assertEquals(obtained, BigInt("6432869891895"))

    test("Day09p2 - readFile - test"):
        val obtained = Day09p2.readFile("./inputs/Day09Test.txt")
        assertEquals(obtained.blocks.size, 19)
        assertEquals(
            obtained.toString,
            "00...111...2...333.44.5555.6666.777.888899"
        )
        assert(obtained.fileSystemCheck)

    test("Day09p2 - findFileIndex"):
        val disk = Day09p2.readFile("./inputs/Day09Test.txt")

        assertEquals(disk.findFileIndex(9), 18)
        assertEquals(disk.findFileIndex(0), 0)
        assertEquals(disk.findFileIndex(2), 4)

    test("Day09p2 - findFileSizes"):
        val disk = Day09p2.readFile("./inputs/Day09Test.txt")

        assertEquals(disk.fileSizes(9), 2)
        assertEquals(disk.fileSizes(2), 1)
        assertEquals(disk.fileSizes(7), 3)

    test("Day09p2 - findFirstFreeSpaceIndex"):
        val disk = Day09p2.readFile("./inputs/Day09Test.txt")

        assertEquals(disk.findFirstFreeSpaceIndex(1), 1)
        assertEquals(disk.findFirstFreeSpaceIndex(2), 1)

    test("Day09p2 - split"):
        val disk = Day09p2.readFile("./inputs/Day09Test.txt")
        disk.split(1, 2)
        val expected = "00...111...2...333.44.5555.6666.777.888899"

        assertEquals(disk.toString, expected)

    test("Day09p2 - swap"):
        val disk = Day09p2.readFile("./inputs/Day09Test.txt")
        disk.split(1, 2)
        assertEquals(
            disk.toString,
            "00...111...2...333.44.5555.6666.777.888899"
        )
        disk.swap(1, 19)
        assertEquals(
            disk.toString,
            "0099.111...2...333.44.5555.6666.777.8888.."
        )

    test("Day09p2 - defragment"):
        val disk = Day09p2.readFile("./inputs/Day09Test.txt")
        disk.defragment(9)
        assertEquals(
            disk.toString,
            "0099.111...2...333.44.5555.6666.777.8888.."
        )
        disk.defragment(8)
        assertEquals(
            disk.toString,
            "0099.111...2...333.44.5555.6666.777.8888.."
        )
        disk.defragment(7)
        assertEquals(
            disk.toString,
            "0099.1117772...333.44.5555.6666.....8888.."
        )
        disk.defragment(6)
        assertEquals(
            disk.toString,
            "0099.1117772...333.44.5555.6666.....8888.."
        )
        disk.defragment(5)
        assertEquals(
            disk.toString,
            "0099.1117772...333.44.5555.6666.....8888.."
        )
        disk.defragment(4)
        assertEquals(
            disk.toString,
            "0099.111777244.333....5555.6666.....8888.."
        )
        disk.defragment(3)
        assertEquals(
            disk.toString,
            "0099.111777244.333....5555.6666.....8888.."
        )
        disk.defragment(2)
        assertEquals(
            disk.toString,
            "00992111777.44.333....5555.6666.....8888.."
        )
        disk.defragment(1)
        assertEquals(
            disk.toString,
            "00992111777.44.333....5555.6666.....8888.."
        )

    test("Day09p2 - part2 - test"):
        val input = Day09p2.readFile("./inputs/Day09Test.txt")
        val obtained = Day09p2.part2(input)
        assertEquals(obtained, BigInt("2858"))

    test("Day09p2 - part2"):
        val input = Day09p2.readFile("./inputs/Day09.txt")
        val obtained = Day09p2.part2(input)
        assertEquals(obtained, BigInt("6467290479134"))
end Day09Test
