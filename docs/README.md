# Habu Lisp Documentation

This directory contains technical design documents and implementation guides for the Habu Lisp compiler and runtime system.

## Quick Start

- **[OPERATORS.md](OPERATORS.md)** - Complete reference for all 66 operators
- **[NEXT_STEPS.md](NEXT_STEPS.md)** - Implementation roadmap and priorities

## Implementation Guides

### Active Development

- **[NEXT_STEPS.md](NEXT_STEPS.md)** - Detailed roadmap for next 10 weeks
  - Runtime integration strategies
  - Tail-call optimization implementation
  - Named-let fixes
  - Timeline estimates (10-week plan)
  - Success criteria for each feature

### Design Documents

- **[codegen/JIT_ARCHITECTURE.md](codegen/JIT_ARCHITECTURE.md)** - JIT multi-versioning design
  - ARM64 codegen architecture overview
  - Function versioning with rep-hints
  - Lisp-level dispatcher strategy
  - Specializing recompile path
  - Integration with existing IR

- **[runtime/GC_IMPROVEMENTS.md](runtime/GC_IMPROVEMENTS.md)** - GC improvement roadmap
  - Current GC architecture analysis
  - Configurable heap sizes
  - Tunable thresholds
  - Write barrier optimization
  - Incremental collection design
  - JIT/GC integration (safepoints)

- **[RUNTIME_INTEGRATION.md](RUNTIME_INTEGRATION.md)** - Runtime heap integration design
  - Inline allocation strategy
  - FFI approaches
  - Standalone runtime options
  - Memory layout specifications

- **[TCO_DESIGN.md](TCO_DESIGN.md)** - Tail-call optimization design
  - Tail position detection
  - Self-recursive TCO implementation
  - Mutual recursion support
  - Code generation strategy

## Reference Documentation

### Operators

**[OPERATORS.md](OPERATORS.md)** - Complete operator reference

Covers all 66 implemented operators:
- **Arithmetic** (12): +, -, *, /, mod, rem, min, max, abs, 1+, 1-, signum
- **Comparison** (7): <, >, =, <=, >=, /=, equal
- **Boolean** (3): and, or, not
- **Bitwise** (7): logand, logior, logxor, lognot, ash, logcount, logtest
- **Predicates** (6): zerop, plusp, minusp, evenp, oddp, null
- **Control Flow** (7): if, cond, case, when, unless, progn, begin
- **Variables** (5): let, let*, setq, incf, decf
- **Functions** (2): lambda, defun
- **Macros** (1): defmacro
- **Data** (5): quote, cons, car, cdr, list
- **Utility** (1): identity

Each entry includes:
- Syntax and examples
- Implementation notes
- Architecture support (x86_64, ARM64)
- Performance characteristics

## Project Root Documentation

Additional documentation in the project root:

- **[../README.md](../README.md)** - Project overview and building
- **[../STATUS.md](../STATUS.md)** - Current implementation status
- **[../DESIGN.md](../DESIGN.md)** - Overall architecture
- **[../COMPILER.md](../COMPILER.md)** - Compiler implementation details
- **[../ROADMAP.md](../ROADMAP.md)** - Long-term development plan
- **[../FULL_LISP_PLAN.md](../FULL_LISP_PLAN.md)** - 15-phase roadmap to production Lisp

## Testing and Benchmarks

- **[../TESTING.md](../TESTING.md)** - Test framework documentation
- **[../TEST_FRAMEWORK_SPEC.md](../TEST_FRAMEWORK_SPEC.md)** - Enhanced testing infrastructure
- **[../BENCHMARKS.md](../BENCHMARKS.md)** - Performance analysis
- **[../BENCHMARK_SPEC.md](../BENCHMARK_SPEC.md)** - Benchmarking framework

## Session Documentation

Implementation session summaries:

- **[../SESSION_CONTEXT.md](../SESSION_CONTEXT.md)** - Complete session history
- **[../SESSION_FINAL_SUMMARY.md](../SESSION_FINAL_SUMMARY.md)** - Final session summary
- **[../SESSION_EXTENDED_SUMMARY.md](../SESSION_EXTENDED_SUMMARY.md)** - Extended technical summary
- **[../SESSION_SUMMARY.md](../SESSION_SUMMARY.md)** - Session summaries

## Current Status

**Compiler**: 134/134 tests passing
**Runtime**: 166/166 tests passing
**Total**: 300/300 tests passing (100%)
**Architectures**: x86_64, ARM64

## Next Priorities

1. **Runtime Integration** (CRITICAL) - Enable cons/car/cdr in compiled code
2. **Tail-Call Optimization** (CRITICAL) - Constant stack space for recursion
3. **Named-Let** (Important) - Fix lambda compilation for local recursion

See [NEXT_STEPS.md](NEXT_STEPS.md) for detailed implementation plans.

## Contributing

When adding new documentation:

1. Update this README with links to new documents
2. Keep operator reference in sync with compiler implementation
3. Update session summaries in project root
4. Use technical language, avoid marketing speak
5. Include code examples and performance data

---

**Last Updated**: 2025-11-18
**Documentation Version**: v4
