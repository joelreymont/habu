# Habu Design Document

## Overview

Habu is a Common Lisp implementation targeting bare-metal ARM64 systems with hard real-time constraints (sub-millisecond latency). The system supports both garbage-collected and GC-free execution modes for mixed real-time workloads.

## Design Goals

1. Sub-millisecond latency in control loops
2. GC-free execution for hard real-time code paths
3. Incremental generational GC for background tasks
4. Minimal runtime footprint (target: <50KB)
5. Maximum code written in Lisp (target: >90%)

## Architecture

### Memory Management

Two independent memory management systems:

1. **Region Allocator** (GC-free)
   - Bump-pointer allocation: O(1)
   - Region reset: O(1)
   - Zero GC pauses
   - For control loops and time-critical code

2. **Generational GC** (incremental)
   - Young generation: Cheney copying
   - Old generation: Incremental mark-sweep
   - For background tasks and non-critical code

### Implementation Strategy

**Phase 1: C Runtime (Hybrid Approach)**
- Memory primitives in C
- GC hot paths in C
- Region allocator in C
- Target: <1000 lines of C

**Phase 2: Lisp Compiler**
- Bootstrap compiler in Common Lisp (SBCL)
- Compiles Habu Lisp to ARM64 machine code
- Direct code generation (no C intermediate)

**Phase 3: Lisp Runtime**
- GC orchestration in Lisp
- Standard library in Lisp
- Eventually self-hosting

## Memory Layout

```
Object Header (64-bit):
┌────────┬──────────┬──────────┬──────────┐
│ Type   │ Size     │ GC Bits  │ Gen Age  │
│ 8 bits │ 40 bits  │ 2 bits   │ 6 bits   │
└────────┴──────────┴──────────┴──────────┘

Tagged Pointers (64-bit):
- Fixnum: [60-bit value][0000]
- Pointer: [60-bit addr][type-tag]
```

## GC Algorithm

### Young Generation
- Algorithm: Cheney copying
- Trigger: Every 512KB allocated
- Pause: ~100-300μs
- Only runs when GC enabled

### Old Generation
- Algorithm: Incremental tri-color mark-sweep
- Work per young GC: Mark/sweep 32 objects
- Pause per increment: ~50-100μs
- Only runs when GC enabled

### Region Memory
- Allocation: Bump pointer
- Deallocation: Region reset
- No GC involvement
- For control loops

## Performance Targets

| Component | Target | Measurement |
|-----------|--------|-------------|
| Region allocation | <10 cycles | Benchmark |
| Young GC pause | <300μs | Max observed |
| Old GC increment | <100μs | Max observed |
| Control loop jitter | <50μs | Std deviation |
| Binary size (Hello World) | <10KB | Stripped binary |

## Testing Strategy

1. Unit tests for all components
2. Benchmarks for performance-critical paths
3. Integration tests for GC scenarios
4. Stress tests for memory management
5. Real-time latency tests

## Development Phases

### Phase 1: Foundation (Week 1)
- Project structure
- Build system
- C runtime primitives
- Test framework
- Region allocator + tests

### Phase 2: GC Implementation (Week 2)
- Object representation
- Mark phase + tests
- Sweep phase + tests
- Young generation + tests
- Benchmarks

### Phase 3: Compiler Bootstrap (Week 3-4)
- S-expression reader
- ARM64 code generator
- Basic runtime functions
- Hello World compilation

### Phase 4: Integration (Week 5)
- Lisp GC orchestration
- Mixed allocation modes
- Control loop example
- Performance validation

## File Organization

```
habu/
├── DESIGN.md              # This file
├── README.md              # User documentation
├── Makefile               # Build system
├── runtime/
│   ├── habu.h             # Public API
│   ├── runtime.c          # Memory, I/O primitives
│   ├── region.c           # Region allocator
│   ├── gc.c               # GC implementation
│   └── object.h           # Object representation
├── bootstrap/
│   ├── habu.asd           # ASDF system
│   ├── compiler.lisp      # Compiler main
│   ├── reader.lisp        # S-expression reader
│   ├── codegen.lisp       # ARM64 code generation
│   └── elf.lisp           # Binary output
├── src/
│   ├── primitives.lisp    # Low-level operations
│   ├── gc.lisp            # GC orchestration
│   └── runtime.lisp       # cons, car, cdr, etc.
├── tests/
│   ├── test_runtime.c     # C runtime tests
│   ├── test_region.c      # Region allocator tests
│   ├── test_gc.c          # GC tests
│   └── run_tests.sh       # Test runner
├── benchmarks/
│   ├── bench_region.c     # Region allocator benchmark
│   ├── bench_gc.c         # GC benchmark
│   └── bench_control.c    # Control loop benchmark
└── examples/
    ├── hello.lisp
    └── drone_control.lisp
```

## Dependencies

- GCC or Clang with ARM64 support
- SBCL (for bootstrap compiler)
- Make
- QEMU (for testing ARM64 on x86)

## Open Questions

1. Virtual vs physical memory on bare metal?
2. Interrupt handling during GC?
3. Multi-core support strategy?
4. Floating-point representation?
