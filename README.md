# scala3-aoc-2024

Here we go again: [Advent of Code - 2024][aoc2024]!

To make this work you need to run ...

```bash
./mill main.test
./mill main.run
```

**Note**: This time around the focus is on learning `scala3`
and using `GitHub Copilot/emacs` and `Codeium/Windsurf` as 
much as possible (as pair programming buddies).

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

[aoc2024]: https://adventofcode.com/2024
