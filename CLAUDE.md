# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System

This project uses **Mill** as the build tool with Scala 3.4.3. The build configuration is in `build.sc`.

### Essential Commands

```bash
# Run all solutions
./mill main.run

# Run tests  
./mill main.test

# Run specific test (use MUnit test names)
./mill main.test "DayXXTest"

# Format code
./mill main.reformat

# Apply scalafix
./mill main.fix

# Generate test coverage report
./mill main.scoverage.htmlReport

# Compile only
./mill main.compile

# Test with specific args (exclude slow tests by default)
./mill main.test.testCached
```

### Pre-push Hook

Install the git pre-push hook to ensure code quality:
```bash
cd ./.git/hooks
ln -s ../../hooks/pre-push .
```

## Architecture Overview

### Project Structure

- `main/src/aoc2024/` - Daily Advent of Code solutions (Day01.scala through Day25.scala)
- `main/src/util/` - Reusable utility classes for common puzzle patterns
- `main/resources/inputs/` - Input files for each day's puzzle
- `main/test/src/` - MUnit test suites

### Key Architectural Patterns

**Daily Solutions**: Each day follows a consistent pattern with:
- `readFile()` method to parse input
- `part1()` and `part2()` methods for each puzzle part
- Solutions are called from `Main.scala` which runs all days sequentially

**Utility Framework**: The `util` package provides reusable components for common AoC patterns:
- `Grid` - 2D grid representation with free/blocked positions, start/end points
- `Position`/`DPosition` - Position handling with directional movement
- `Bfs`/`Dfs` - Search algorithms for pathfinding
- `GridGraph`/`WDGridGraph` - Graph representations for grid-based puzzles
- `Path` - Path tracking and scoring utilities

**Grid-Based Puzzles**: Many solutions use the Grid utility for 2D navigation problems. The Grid class handles:
- Parsing grid files with obstacles (#), free spaces (.), start (S), and end (E) positions
- Boundary checking and neighbor finding
- Integration with search algorithms

**Dependencies**: Key external libraries used:
- `scala-graph` - Graph algorithms and shortest path finding
- `breeze` - Linear algebra (for solving equation systems)
- `scala-corner` - Counting corners in 2D regions
- `scala-parallel-collections` - Parallel processing
- `munit` - Testing framework

### Testing Strategy

Tests use MUnit with:
- Test data files in `main/resources/inputs/` (e.g., `Day01Test.txt`)
- Separate test classes for each day in `main/test/src/aoc2024/`
- Utility tests in `main/test/src/util/`
- Tests are tagged and can be run selectively (slow tests excluded by default)

### Code Style

- Uses Scala 3 indent-based syntax (migrated from braces)
- Scalafmt for formatting, Scalafix for linting
- Compiler warnings treated as errors (`-Xfatal-warnings`)
- Unused imports detection enabled