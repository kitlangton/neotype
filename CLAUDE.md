# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

The project uses sbt (Scala Build Tool). Key commands:

```bash
# Compile all modules
sbt compile

# Run all tests
sbt test

# Run a specific test
sbt "testOnly *SpecificSpec"

# Continuous compilation (watches for file changes)
sbt ~compile

# Format code and run linting (scalafmt + scalafix)
sbt prepare

# Clean build artifacts
sbt clean
```

## Architecture Overview

Neotype is a Scala 3 library providing zero-cost newtype abstractions with compile-time validation. The project uses:

- **Opaque types** for zero runtime overhead
- **Inline methods and macros** for compile-time validation
- **Cross-platform support**: JVM, JS (Scala.js), and Native

### Module Structure

The codebase is organized into:

1. **Core module** (`modules/core/`): Contains the main `Newtype` and `Subtype` abstractions
2. **Integration modules** (`modules/neotype-*/`): Library-specific integrations (circe, zio-json, doobie, etc.)

Each integration module follows the pattern:
- `Main.scala`: Contains given instances for the target library
- `*Spec.scala`: Integration tests using ZIO Test

### Key Abstractions

The library provides two main abstractions:

1. **Newtype[A]**: Creates a new type with the same runtime representation as A
2. **Subtype[A]**: Creates a subtype that can be used where A is expected

Both support:
- Compile-time validation via `inline def validate`
- Runtime validation via `def make`
- Custom error messages

### Testing Approach

Tests use ZIO Test framework. When writing tests:
- Use `typeCheckErrors` to test compile-time validation
- Test both successful and failure cases for runtime validation
- Integration tests should verify codec/instance behavior

### Development Patterns

When adding new features or integrations:
1. All new modules go under `modules/neotype-{library}/`
2. Follow the existing integration pattern with given instances
3. Add both unit and integration tests
4. Ensure cross-platform compatibility where applicable