# CLAUDE.md

This file provides guidance to Claude Code when working with this repository. For basic usage information, see [README.md][]. For contribution guidelines, see [CONTRIBUTING.md][].

## Architecture and Design Decisions

### Why Mill?

Mill was chosen as the build tool over SBT because:
- Simpler, more predictable configuration (plain Scala instead of DSL)
- Faster builds with better caching
- Better integration with modern Scala 3 tooling
- Easier to understand for developers new to Scala

The build configuration lives in `build.sc` with Scala 3.4.3 as the target version.

### Why This Project Structure?

**Daily Solutions Pattern**: Each day follows a consistent structure because:
- `readFile()` isolates input parsing from algorithm logic
- `part1()` and `part2()` methods allow independent testing
- `Main.scala` orchestrates execution, making it easy to run all solutions or specific days
- This separation enables better testing and code reuse

**Utility Framework Philosophy**: The `util` package was designed to:
- Extract common patterns that appear across multiple puzzles
- Avoid premature abstraction (utilities are added only after patterns emerge)
- Provide composable building blocks rather than monolithic solutions
- Maintain type safety while keeping the API ergonomic

### Key Design Patterns

**Grid System**: Many AoC puzzles involve 2D grids, so we built a comprehensive grid framework:
- `Grid` class provides a high-level abstraction for grid navigation
- `Position`/`DPosition` handle coordinates with directional movement
- `GridGraph`/`WDGridGraph` convert grids to graph representations for pathfinding
- This design separates spatial representation from algorithm implementation

**Search Algorithms**: BFS and DFS are implemented separately because:
- Different puzzles need different traversal orders
- Keeping them separate makes the code easier to understand and test
- Both integrate with the Grid system through common interfaces

**Path Tracking**: The `Path` utility emerged from puzzles requiring:
- Score accumulation during traversal
- History tracking for constraint checking
- Backtracking for finding all valid paths

### Dependency Choices

**scala-graph**: Used for shortest path and graph algorithms because:
- Provides battle-tested implementations
- Integrates well with Scala's type system
- Supports weighted graphs needed for many puzzles

**breeze**: Chosen for linear algebra because:
- Some puzzles require solving equation systems
- Well-maintained library with good performance
- Familiar API for developers with NumPy experience

**scala-corner**: Specialized library for counting corners in 2D regions because:
- This specific geometric operation appears in multiple puzzles
- Implementing it correctly is non-trivial
- Using a library reduces bug risk

**munit**: Selected as testing framework because:
- Lightweight and fast
- Good Scala 3 support
- Simple assertion syntax

### Testing Philosophy

Tests are structured with:
- Separate test data files to isolate test inputs from production code
- One test class per day for organization
- Tagged tests to exclude slow-running tests from regular CI
- Target of 80% code coverage to balance thoroughness with pragmatism

### Code Style Decisions

**Indent-based syntax**: Migrated from braces to indent because:
- More idiomatic in Scala 3
- Reduces visual noise
- Encourages better code structure through indentation awareness

**Strict compiler settings**: Warnings treated as errors because:
- Forces addressing issues immediately
- Prevents accumulation of technical debt
- Maintains high code quality throughout development

**Formatting and linting**: Automated with scalafmt and scalafix because:
- Removes subjective style debates
- Ensures consistency across the codebase
- Catches common mistakes early

### Evolution and Migration Notes

The codebase originally used brace-based syntax and was migrated to indent-based using:
- `./mill main.migrate.compile` for automated conversion
- Manual cleanup with scalafmt and scalafix
- This migration taught valuable lessons about Scala 3's syntax flexibility

[README.md]: README.md
[CONTRIBUTING.md]: CONTRIBUTING.md