# scala3-aoc-2024

Here we go again: [Advent of Code - 2024][aoc2024]!

To make this work you need to run ...

```bash
./mill main.test
./mill main.run
```

**Note**: This time around the focus is on learning `scala3`
and using `GitHub Copilot/emacs` and `Codeium/Windsurf` as 
much as possible (as a pair programming buddy).

**Note**: Initially the (scala3) code was formatted with 
braces. Then I decided to switch to indent. To migrate 
the code I did run `./mill main.migrate.compile` and
made heavy use of `scalafmt` and `scalafix`.

**Note**: You can install a/the pre-push hook with ...
```bash
cd ./.git/hooks
ln -s ../../hook/pre-push .
```
The hook will make sure the code is properly formatted
and linted before each push.

## What I have learned?

- Lot's about Scala3 ...
  - extensions
  - given and using
  - indent vs. braces
  - ... and (much) more
- Using `scalafmt` (for Scala3)
- Using `scalafix` (for Scala3)
- Using `breeze` to solve linear equation systems
- Using `scala-graph` to find the shortest path in a graph (or 2-dim grid)
- Using `scala-corner` to count the number of corners of regions in a 2-dim grid
- Using `Codeium` and `Windsurf` to write code

[aoc2024]: https://adventofcode.com/2024
