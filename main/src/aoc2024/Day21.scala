package aoc2024

import com.typesafe.scalalogging.Logger

/** Day21 - Keypad Conundrum
  *
  * OMG ... this sounds ... interesting.
  *
  * Let's start with finding the shortest path on the numeric keypad (using the (first) directional
  * keypad). Will use scala-graph for that.
  *
  * Interesting ... there are 3 possible (shortest) paths for the first code (029A) on the numeric
  * keypad. Initially they have the same cost (the length of the path).
  *
  * 029A: (<A)(^A)(>^^A)(vvvA)
  *
  * Note: This path is constructed from the sum of the length of the subpaths. Every subpath starts
  * were the previous one ends.
  *
  * But when we try to type this by means of the directional keypad the edges suddenly have a
  * different cost.
  *
  * To do a < on the numeric keypad, we have to type v<<A on the directional keypad. For a ^ we only
  * have to type <A.
  *
  * To type 029A/<A^A>^^AvvvA on the numeric keypad, we have to type
  * (v<<A)(>>^A)(<A)(>A)(vA)(<^A)(A)(>A)(<vA)(A)(A)(>^A) on the directional keypad.
  *
  * This is bad, because the cost of a move/edge is different all the time. And I mean, not only
  * that the cost of < is different from the cost of >, but also that the cost of two < can be
  * different (e.g. if they are subsequent moves, the first one will take x moves to get the < key
  * and then hit A. For the second one we just have to hit A again).
  *
  * Means the cost of the current edge needs to be derived from the previous move and the current
  * move, e.g. if previous == current cost == 1 (because we just need to hit A again to get the
  * next/same move again).
  *
  * The cost of the edges of AvvvA is Av/<vA:3 vv/A:1 vv/A:1 vA/>^A:3. So the total cost is 8.
  *
  * Note: The cost of x/x is always 1 and the cost of x/y is equal to the cost of y/x (symmetric).
  *
  * Hhhmmm ... that sounds not too bad. We can probably create a map to represent the cost of all
  * possible edge pairs.
  *
  * But wait ... there is more ...
  *
  * We now need to do this for 2 more directional keypads.
  *
  * At the end we are looking at 3 directional keypads and 1 numeric keypad.
  *
  * me: directional keypad: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
  * robot1: directional keypad: v<<A>>^A<A>AvA<^AA>A<vAAA>^A robot2: directional keypad:
  * <A^A>^^AvvvA robot3: numeric keypad: 029A
  *
  * I am starting to think about a different approach.
  *
  * Maybe it is simpler. Maybe we can come up with the shortest path and then just use "string
  * replace" to come up with the other inputs.
  *
  * The insight here is that every compination of two moves can be mapped to a Seq of moves on the
  * next directional keypad. For instance ...
  *
  *   - A< becomes v<<A
  *   - >A becomes >>^A
  *   - vv becomes A
  *
  * Note: A< does NOT become <v<A, because the next level down we miss the chance to just hit A
  * twice to go left twice.
  *
  * Note: For the 4 keys that are arranged in a 2x2 grid (^A and v>) we also have to decide how to
  * get to the diagonal keys. Clockwise or counterclockwise. Up/Down first or Left/Right first. For
  * now I will go with clockwise.
  *
  * Note: The numeric keypad has no diagonal keys.
  *
  * And then you apply the same replacements to resulting string again.
  */

object Day21:

  import scalax.collection.mutable
  import scalax.collection.generic

  val logger: Logger = Logger(this.getClass.getName)

  type Code = String

  /** @return the file for the given filename as parsed elements */
  def readFile(filename: String): Set[Code] =
    import scala.io.Source

    require(filename.nonEmpty, "filename.nonEmpty")
    logger.debug(s"filename: ${filename}")

    val source = Source.fromResource(filename)
    try source.getLines().toSet
    finally source.close()
  end readFile

  case class NumericKey(key: Char)

  case class NumericEdge(from: NumericKey, to: NumericKey, move: Char)
      extends generic.AbstractDiEdge(from, to)

  type NumericKeypad = mutable.Graph[NumericKey, NumericEdge]

  extension (pad: NumericKeypad)

    def path0(from: Char, to: Char): String =
      logger.debug(s"from: ${from}, to: ${to}")

      def ordering(e: pad.EdgeT): Float = e.move match
        case '<' => 1.0f
        case '>' => 2.0f
        case 'v' => 3.0f
        case '^' => 4.0f

      val sp = pad.get(NumericKey(from)).shortestPathTo(pad.get(NumericKey(to)), ordering)
      sp.get.edges.map(_.move).mkString // scalafix:ok
    end path0

    def path(keys: String): String =
      val (pairs, _) = keys.foldLeft(Set.empty[(Char, Char)], 'A'):
        case ((ps, previous), current) => (ps ++ Set((previous, current)), current)
      pairs.map:
        case (from, to) => pad.path0(from, to)
      .mkString("", "A", "A")
    end path

  end extension

  object NumericKeypad extends mutable.TypedGraphFactory[NumericKey, NumericEdge]:

    val padEdges: Set[(Char, Char, Char)] = Set(
      ('7', '8', '>'),
      ('7', '4', 'v'),
      ('8', '7', '<'),
      ('8', '5', 'v'),
      ('8', '9', '>'),
      ('9', '6', 'v'),
      ('9', '8', '<'),
      ('4', '5', '>'),
      ('4', '1', 'v'),
      ('4', '7', '^'),
      ('5', '6', '>'),
      ('5', '2', 'v'),
      ('5', '4', '<'),
      ('5', '8', '^'),
      ('6', '3', 'v'),
      ('6', '9', '^'),
      ('6', '5', '<'),
      ('1', '2', '>'),
      ('1', '4', '^'),
      ('2', '3', '>'),
      ('2', '0', 'v'),
      ('2', '1', '<'),
      ('2', '5', '^'),
      ('3', '6', '^'),
      ('3', 'A', 'v'),
      ('3', '2', '<'),
      ('0', 'A', '>'),
      ('0', '2', '^'),
      ('A', '0', '<'),
      ('A', '3', '^'),
    )

    def create: NumericKeypad = NumericKeypad.from(
      padEdges.map: edge =>
        val (from, to, move) = edge
        NumericEdge(NumericKey(from), NumericKey(to), move)
    )

  end NumericKeypad

  val lookup: Map[String, Set[String]] = Map(
    "<<" -> Set("A"),
    "<v" -> Set(">A"),
    "<>" -> Set(">>A"),
    "<^" -> Set(">^A"),
    "<A" -> Set(">>^A"),
    // manual optimization "<A" -> Set(">>^A", ">^>A"),

    "vv" -> Set("A"),
    "v<" -> Set("<A"),
    "v>" -> Set(">A"),
    "v^" -> Set("^A"),
    "vA" -> Set("^>A", ">^A"),
    ">>" -> Set("A"),
    ">v" -> Set("<A"),
    "><" -> Set("<<A"),
    ">A" -> Set("^A"),
    ">^" -> Set("^<A", "<^A"),
    "^^" -> Set("A"),
    "^v" -> Set("vA"),
    "^<" -> Set("v<A"),
    "^A" -> Set(">A"),
    "^>" -> Set("v>A", ">vA"),
    "AA" -> Set("A"),
    "A^" -> Set("<A"),
    "A>" -> Set("vA"),
    "Av" -> Set("v<A", "<vA"),
    "A<" -> Set("v<<A"),
    // manual optimization "A<" -> Set("v<<A", "<v<A"),
  )

  /** @return the list of all possible keystrokes sequences */
  def next(
      keys: String,
      previous: Char = 'A',
      keystrokes: String = "",
      collector: Set[String] = Set.empty,
  ): Set[String] =
    if keys.isEmpty then collector + keystrokes
    else
      val nextKeys = lookup(s"${previous}${keys.head}")
      nextKeys.foldLeft(collector): (c, nextKey) =>
        next(keys.tail, keys.head, keystrokes + nextKey, c)
    end if
  end next

  /** @return the length of the shortest keystroke sequence (using N directional keypads */
  def level(keys: String, n: Int): Int =
    if n <= 0 then next(keys).map(_.size).min
    else
      next(keys).foldLeft(Int.MaxValue): (min, nk) =>
        level(nk, n - 1).min(min)
  end level

  /** @return the complexity score for the keystroke sequence */
  def complexity(keys: String, n: Int): Int = keys.take(3).toInt * n

  /** @return the sum of the complexity scores */
  def part1(codes: Set[Code]): Int =
    require(codes.nonEmpty, "codes.nonEmpty")
    logger.debug(s"codes: ${codes}")

    val numericKeypad = NumericKeypad.create
    val complexities = codes.map: code =>
      val keys = numericKeypad.path(code)
      val shortestPathLength = level(keys, 1)
      logger.info(s"code: ${code}, keys: ${keys}, shortestPathLength: ${shortestPathLength}")
      complexity(code, shortestPathLength)

    complexities.sum
  end part1

  /** @return the solution for part2 */
  def part2(codes: Set[Code]): Int =
    require(codes.nonEmpty, "codes.nonEmpty")
    logger.debug(s"codes: ${codes}")

    codes.map(_.size).sum
  end part2

end Day21
