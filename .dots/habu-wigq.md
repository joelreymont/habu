---
title: Add GC threshold tuning API
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-06T06:33:04.615729+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From GC analysis: gc_threshold is hard-coded to YOUNG_GEN_SIZE/2 (256KB). For dynamic heap sizes and JIT workloads, this should be tunable.

## Goal

Expose GC threshold as a runtime-configurable parameter for experimentation and tuning.

## Implementation

### C API

```c
// In runtime/gc.c
void gc_set_young_threshold(size_t bytes) {
    gc_heap->gc_threshold = bytes;
}

size_t gc_get_young_threshold(void) {
    return gc_heap->gc_threshold;
}
```

### Lisp API

```lisp
;; In runtime/memory.lisp
(defun set-gc-threshold (bytes)
  "Set young generation GC threshold in bytes"
  (sys-gc-set-threshold bytes))

(defun get-gc-threshold ()
  "Get current young generation GC threshold"
  (sys-gc-get-threshold))
```

### Environment Variable Support

```c
// In init(), check environment
char *threshold_env = getenv("HABU_GC_YOUNG_THRESHOLD");
if (threshold_env) {
    size_t threshold = atoi(threshold_env);
    gc_heap->gc_threshold = threshold;
}
```

## Use Cases

1. **JIT tuning**: Adjust threshold when generating lots of temporary objects
2. **Interactive REPL**: Lower threshold for more responsive GC
3. **Batch processing**: Higher threshold for throughput
4. **Profiling**: Experiment with different values

## Tasks

1. Add gc_set_young_threshold / gc_get_young_threshold to runtime/gc.c
2. Export from habu.h
3. Add Lisp wrappers in runtime/memory.lisp
4. Support HABU_GC_YOUNG_THRESHOLD environment variable
5. Document in docs/runtime/GC_RUNTIME.md

## Integration with JIT

When JIT starts creating many specialized function versions:
- Many short-lived function-version objects
- May want to adjust threshold based on compilation phase
- API allows experimentation without recompiling

## References

- /Users/joel/Downloads/habu-jit-gc-package/Habu_GC_Analysis.md
- Section 2.2: "Tune GC thresholds and export them"
