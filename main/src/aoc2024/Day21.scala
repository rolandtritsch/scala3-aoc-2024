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
  * Means the cost of the current edge needs to favor going straight.
  *
  * Hhhmmm ... that sounds not too bad. We can probably create cost function to take that into
  * consideration.
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
  *   - A< becomes v<<A or <v<A
  *   - >A becomes >>^A or >^>A
  *   - vv becomes A
  *
  * And then you apply the same replacements to the resulting string again (N times).
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

      // Cost function for the edges.
      def ordering(e: pad.EdgeT): Float =
        // This is where the magic happens. If the current edge (e.g. between 1 and 2) is in the same
        // row or column as the target (e.g. 3), then the cost-factor is 0. Otherwise it is 1.
        def adjuster(current: Set[Char], lines: Set[Set[Char]]): Float =
          if lines.exists(current.subsetOf(_)) then 0.0f else 1.0f

        val rows = Set(Set('1', '2', '3'), Set('4', '5', '6'), Set('7', '8', '9'), Set('0', 'A'))
        val cols = Set(Set('1', '4', '7'), Set('0', '2', '5', '8'), Set('A', '3', '6', '9'))
        val current = Set(e.source.key, e.target.key, to)

        // Favoring going horizontal, then vertical, but in any case straight
        e.move match
          case '<' => 1.0f * adjuster(current, rows)
          case '>' => 2.0f * adjuster(current, rows)
          case 'v' => 3.0f * adjuster(current, cols)
          case '^' => 4.0f * adjuster(current, cols)
        end match
      end ordering

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
      ('0', '2', '^'),
      ('0', 'A', '>'),
      ('1', '2', '>'),
      ('1', '4', '^'),
      ('2', '0', 'v'),
      ('2', '1', '<'),
      ('2', '3', '>'),
      ('2', '5', '^'),
      ('3', '2', '<'),
      ('3', '6', '^'),
      ('3', 'A', 'v'),
      ('4', '1', 'v'),
      ('4', '5', '>'),
      ('4', '7', '^'),
      ('5', '2', 'v'),
      ('5', '4', '<'),
      ('5', '6', '>'),
      ('5', '8', '^'),
      ('6', '3', 'v'),
      ('6', '5', '<'),
      ('6', '9', '^'),
      ('7', '4', 'v'),
      ('7', '8', '>'),
      ('8', '5', 'v'),
      ('8', '7', '<'),
      ('8', '9', '>'),
      ('9', '6', 'v'),
      ('9', '8', '<'),
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
    "<A" -> Set(">>^A", ">^>A"),
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
    "A<" -> Set("v<<A", "<v<A"),
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

  /** @return the length of the shortest keystroke sequence (using N directional keypads) */
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
