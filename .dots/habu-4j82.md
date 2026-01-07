---
title: Make GC heap size configurable
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-06T06:32:50.768668+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From GC analysis: runtime/gc.c has init(size_t heap_size) that currently ignores its parameter and uses fixed sizes (512KB young, 4MB old = ~5MB total).

## Problem

Fixed heap size is limiting for:
- Larger programs/datasets
- Stress testing (e.g., 256MB heap)
- Embedded scenarios (smaller footprint)

## Solution (from HABU_GC_IMPROVEMENTS.md)

Interpret heap_size as total heap budget and derive generation sizes dynamically.

### Implementation

```c
void init(size_t heap_size) {
    // Derive generation sizes from total budget
    size_t young_bytes = heap_size / 5;      // 20% of heap
    size_t old_bytes = heap_size - young_bytes;
    
    // Ensure minimums and alignment
    young_bytes = MAX(young_bytes, 256 * 1024);
    old_bytes = MAX(old_bytes, 2 * 1024 * 1024);
    
    // Replace YOUNG_GEN_SIZE and OLD_GEN_SIZE with runtime fields
    gc_heap->young_gen_size = young_bytes;
    gc_heap->old_gen_size = old_bytes;
    
    // Allocate accordingly
    // ...
}
```

## Changes Required

1. Add young_gen_size and old_gen_size fields to gc_heap_t
2. Replace YOUNG_GEN_SIZE/OLD_GEN_SIZE constants with runtime values
3. Update all size calculations to use runtime fields
4. Update gc_threshold calculation to use runtime young_gen_size

## Benefits

- Configurable heap for different workloads
- Better testing (can stress with larger heaps)
- Production flexibility
- No algorithm changes - just configuration

## Tasks

1. Update gc_heap_t struct with size fields
2. Modify init() to compute sizes from heap_size parameter
3. Replace all YOUNG_GEN_SIZE/OLD_GEN_SIZE references
4. Test with various heap sizes (1MB, 16MB, 256MB)
5. Update documentation in docs/runtime/GC_RUNTIME.md

## File

- runtime/gc.c (lines 277-340 for init function)

## References

- /Users/joel/Downloads/habu-jit-gc-package/Habu_GC_Analysis.md
- Section 2.1: "Honor the heap_size parameter"
