# Habu Testing Guide

## Test Suite

Habu includes comprehensive test coverage across all major components.

### Test Categories

**Platform Tests** (`tests/test_platform.c`)
- Platform detection and verification
- Type size validation
- Pointer tagging correctness
- Fixnum range validation
- Endianness detection
- Clock resolution measurement
- Memory allocation verification

**Region Allocator Tests** (`tests/test_region.c`)
- Basic allocation and deallocation
- Alignment verification
- Exhaustion handling
- Reset functionality
- Object construction (cons, vector, string)
- Mixed allocation patterns

**Garbage Collector Tests** (`tests/test_gc.c`)
- Initialization and shutdown
- Object allocation for all types
- Nested structures
- Collection cycles
- GC enable/disable modes
- Heap usage tracking
- Statistics collection

### Running Tests

```bash
# Run all tests
make test

# Run individual test suites
./tests/test_platform
./tests/test_region
./tests/test_gc

# Clean and rebuild
make clean && make test
```

### Test Results: Linux x86_64

**Environment:**
- Platform: x86_64
- OS: Linux
- Compiler: GCC 13.3.0
- Pointer size: 8 bytes
- Endianness: little
- Clock resolution: ~152 ns

**Test Results:**
```
Platform verification: 10/10 tests passed
Region allocator:      12/12 tests passed
Garbage collector:     15/15 tests passed
────────────────────────────────────────
Total:                 37/37 tests passed
```

**Benchmark Results:**
```
Region allocation:
  Small (16 bytes):      1.25 ns/alloc
  Medium (256 bytes):    1.10 ns/alloc
  Cons:                  20.97 ns/cons
  Control loop:          37.41 ns average (27 ns stddev)

GC allocation:
  Cons (GC disabled):    4.55 ns/cons
  Vector (10 elements):  39.39 ns/vector
  GC collection:         0.31 us (1000 objects)

Control loop demo:
  Average iteration:     95.53 ns
  Min:                   90 ns
  Max:                   400 ns
  Status:                PASS (<1 ms target)
```

### Platform-Specific Notes

**Linux x86_64:**
- All tests passing
- Performance excellent for development/testing
- Used for CI and local development

**macOS ARM64:**
- Primary development target
- Expected similar or better performance
- Native platform for bare-metal ARM64 development

**ARM64 Linux (cross-compiled):**
- Cross-compilation working
- Runtime testing pending
- Target for future embedded Linux deployments

## Adding New Tests

Follow the existing test structure:

```c
/* Test description */

#include "../runtime/habu.h"
#include <stdio.h>
#include <assert.h>

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) \
    static void test_##name(void); \
    static void run_test_##name(void) { \
        tests_run++; \
        printf("  %s... ", #name); \
        fflush(stdout); \
        test_##name(); \
        tests_passed++; \
        printf("ok\n"); \
    } \
    static void test_##name(void)

#define RUN_TEST(name) run_test_##name()

TEST(your_test_name) {
    // Test implementation
    assert(condition);
}

int main(void) {
    printf("Your test suite:\n");
    RUN_TEST(your_test_name);
    printf("\n%d/%d tests passed\n", tests_passed, tests_run);
    return tests_passed == tests_run ? 0 : 1;
}
```

Add to `Makefile`:
```make
TEST_SRCS = tests/test_platform.c tests/test_region.c tests/test_gc.c tests/test_your_new_test.c
```

## Continuous Integration

Tests are designed to run in CI environments:

```bash
#!/bin/bash
# CI test script
make clean
make test
make benchmark
make examples
./examples/drone_control_demo
```

Exit code 0 indicates all tests passed.

## Debugging Failed Tests

**Compile with debug symbols:**
```bash
make clean
make CFLAGS="-std=c11 -Wall -Wextra -Werror -O0 -g"
```

**Run under debugger:**
```bash
gdb ./tests/test_platform
(gdb) run
(gdb) bt
```

**Enable verbose output:**
Tests print progress as they run. Failed assertions show line numbers.

## Performance Testing

See `benchmarks/` directory for detailed performance benchmarks:

```bash
make benchmark
./benchmarks/bench_region   # Region allocator performance
./benchmarks/bench_gc       # GC performance
```

## Coverage

Current test coverage focuses on:
- Correctness of all public APIs
- Platform compatibility
- Performance validation
- Real-time constraint verification
- Memory safety

Not yet covered:
- Error conditions (out of memory, invalid inputs)
- Thread safety (not yet implemented)
- Signal handling (not yet implemented)
- Bare-metal specific features (future)
