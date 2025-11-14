# scala3-aoc-2024

![aoctree][]

Solutions to [Advent of Code 2024][aoc2024] challenges implemented in Scala 3.

## What is this?

This repository contains my solutions to the [Advent of Code 2024][aoc2024] programming puzzles. Advent of Code is an annual event featuring daily programming challenges throughout December. Each day presents a two-part puzzle that can be solved using any programming language.

This implementation focuses on:
- Learning Scala 3 features and idioms
- Building reusable utilities for common puzzle patterns
- Exploring AI-assisted development with GitHub Copilot and Codeium

## Quick Start

### Prerequisites

- Java 11 or higher
- [Mill build tool][mill] (downloaded automatically via `./mill`)

### Running Solutions

```bash
# Run all solutions
./mill main.run

# Run tests
./mill main.test
```

### Project Structure

- `main/src/aoc2024/` - Daily puzzle solutions (Day01.scala through Day25.scala)
- `main/src/util/` - Reusable utility classes for common patterns
- `main/resources/inputs/` - Puzzle input files
- `main/test/src/` - Test suites with sample data

## Implementation Details

For information about the architecture, design decisions, and implementation patterns, see [CLAUDE.md][].

## Contributing

Interested in contributing? Check out the [CONTRIBUTING.md][] file for development workflow and guidelines.

## What I Learned

- Scala 3 features: extensions, given/using, indent-based syntax
- Mill build system and Scala tooling (scalafmt, scalafix)
- Graph algorithms with `scala-graph`
- Linear algebra with `breeze`
- Geometry algorithms with `scala-corner`
- AI-assisted development workflows

[aoc2024]: https://adventofcode.com/2024
[aoctree]: assets/aoc-tree.png
[mill]: https://mill-build.org
[CLAUDE.md]: CLAUDE.md
[CONTRIBUTING.md]: CONTRIBUTING.md
