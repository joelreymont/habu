# Runtime Garbage Collector

## Overview

The Habu C runtime implements a **generational copying/mark-sweep garbage collector** with:
- **Young generation**: Cheney-style semi-space copying collector (512 KB)
- **Old generation**: Mark-sweep with compaction (4 MB)
- **Write barriers**: Track old→young pointers via remembered set
- **Automatic promotion**: Objects surviving 5+ young collections promoted to old gen
- **Root registration API**: Explicit root management for stack/global values

## Architecture

### Memory Layout

```
┌─────────────────────────────────────────────┐
│  Young Generation (Copying Collector)       │
│  ┌──────────────┬──────────────┐           │
│  │  From Space  │   To Space   │  512KB ea │
│  │   (active)   │   (reserve)  │           │
│  └──────────────┴──────────────┘           │
├─────────────────────────────────────────────┤
│  Old Generation (Mark-Sweep)                │
│  ┌──────────────────────────────┐          │
│  │  Tenured Objects (4 MB)      │          │
│  └──────────────────────────────┘          │
└─────────────────────────────────────────────┘
```

### Object Header

Every heap object has a 16-byte header:

```c
typedef struct {
    uint64_t type : 8;       /* Object type (TYPE_CONS, TYPE_VECTOR, etc) */
    uint64_t size : 40;      /* Payload size in bytes */
    uint64_t gc_color : 2;   /* 0=WHITE, 1=GRAY, 2=BLACK, 3=FORWARDED */
    uint64_t gen_age : 6;    /* Generation age (0-5=young, 6+=old) */
    uint64_t reserved : 8;   /* Reserved for future use */
    uint64_t padding;        /* Forwarding pointer during GC */
} habu_header_t;
```

## Young Generation Collection

### Cheney Algorithm

1. **Swap semispaces**: Flip from-space and to-space
2. **Copy roots**: Evacuate root objects to to-space
3. **Scan remembered set**: Copy young objects referenced from old gen
4. **Scan to-space**: Update all internal pointers
5. **Reclaim**: From-space is now garbage, ready for next cycle

### Promotion

Objects are promoted to old generation when:
- `gen_age >= GEN_YOUNG_MAX` (5 survivals)
- To-space is full (emergency promotion)

```c
if (header->gen_age >= GEN_YOUNG_MAX) {
    /* Promote to old generation */
    new_addr = gc_heap->old_free;
    gc_heap->old_free += obj_size;
    gc_heap->old_bytes_allocated += obj_size;
}
```

### Forwarding Pointers

During copying, the old location stores a forwarding pointer:
- `gc_color = 3` (FORWARDED flag)
- `padding` field stores new address

This prevents duplicate copying when multiple pointers reference the same object.

## Old Generation Collection

### Mark-Sweep-Compact

1. **Mark phase**:
   - Mark from roots
   - Process gray stack until empty
   - All reachable objects marked BLACK

2. **Sweep-Compact phase**:
   - Scan old generation linearly
   - Dead objects (WHITE) are skipped
   - Live objects compacted toward start of old gen
   - Update `old_free` pointer

```c
while (scan < old_free) {
    if (get_gc_color(obj) == GC_WHITE) {
        freed += obj_size;  /* Dead */
    } else {
        memmove(compact_to, scan, obj_size);  /* Compact */
        compact_to += obj_size;
    }
    scan += obj_size;
}
```

### Triggering

Old generation collection triggers when:
- Old generation is full during promotion
- Old generation is full during direct allocation

## Write Barriers

### Purpose

Generational collectors need to track **old→young pointers** to ensure young objects referenced only from old generation are not collected.

### Implementation

Write barriers are inserted at all pointer stores:

```c
void habu_set_car(habu_value_t cons, habu_value_t value) {
    habu_cons_t *c = value_to_cons(cons);
    c->car = value;
    habu_write_barrier(c, value);  /* Track old→young */
}
```

The write barrier checks:
```c
if (in_old_gen(obj) && !in_old_gen(target)) {
    add_to_remembered_set(obj);
}
```

### Remembered Set

- Dynamically sized array of old gen objects with young pointers
- Scanned during young generation collection as additional roots
- Cleared after each young collection

## Root Registration API

### Usage

Register roots before GC, unregister when no longer needed:

```c
habu_value_t obj = habu_cons(x, y);
void *obj_ptr = untag_pointer(obj);

habu_gc_add_root(obj_ptr);     /* Protect from GC */
// ... allocate more objects ...
habu_gc_remove_root(obj_ptr);  /* No longer needed */
```

### Implementation

- Dynamically sized roots array (starts at 256, doubles when full)
- Duplicate detection on add
- Linear search on remove (acceptable for small root sets)

## Allocation Strategy

### Flow

1. **Try young generation**
   - Fast bump-pointer allocation
   - Check if GC threshold exceeded → trigger young GC

2. **Young gen full?**
   - Trigger young GC
   - Retry young allocation
   - Still full? → try old gen

3. **Try old generation**
   - Allocate with `gen_age = GEN_OLD`
   - Old gen full? → trigger old GC
   - Retry old allocation

4. **Heap exhausted**
   - Return NULL (out of memory)

### GC Threshold

Young GC triggers when:
```c
bytes_allocated_since_gc >= gc_threshold  /* Default: 256 KB */
```

This provides a balance between GC frequency and throughput.

## Statistics

### Tracked Metrics

```c
typedef struct {
    uint64_t young_collections;   /* Young GC count */
    uint64_t old_collections;     /* Old GC count */
    uint64_t total_allocated;     /* Bytes allocated */
    uint64_t total_freed;         /* Bytes reclaimed */
    uint64_t max_pause_ns;        /* Max GC pause */
    uint64_t last_pause_ns;       /* Last GC pause */
} habu_gc_stats_t;
```

### API

```c
habu_gc_get_stats(&stats);   /* Get statistics */
habu_gc_reset_stats();       /* Reset counters */
```

## Performance Characteristics

### Young Generation Collection

- **Pause time**: O(live set size)
- **Throughput**: Very high for short-lived objects
- **Typical pause**: 0.1 - 1 ms

### Old Generation Collection

- **Pause time**: O(old gen live set)
- **Throughput**: Lower due to mark-sweep overhead
- **Typical pause**: 1 - 10 ms (depends on old gen occupancy)

### Generational Hypothesis

Most objects die young, so:
- **95%+ objects** collected in fast young GC
- **<5% objects** promoted to slower old GC
- **Overall throughput** much better than single-generation collector

## Testing

New tests validate:

1. **Root registration** (`test_gc.c:197-215`)
   - Register root, trigger GC, verify object survives

2. **Promotion** (`test_gc.c:217-236`)
   - Trigger 10 GCs, verify object promoted and survives

3. **Write barrier** (`test_gc.c:238-264`)
   - Create old object, store young object, verify young survives GC

4. **Old generation collection** (`test_gc.c:266-291`)
   - Fill old gen, verify old GC triggers

## API Reference

### Initialization

```c
void habu_init(size_t heap_size);
void habu_shutdown(void);
```

### Memory Management Modes

```c
void habu_enable_gc(void);   /* Enable automatic GC */
void habu_disable_gc(void);  /* Disable automatic GC */
```

### Explicit GC Control

```c
void habu_gc_collect(void);       /* Force young GC */
size_t habu_gc_heap_size(void);   /* Total heap size */
size_t habu_gc_heap_used(void);   /* Bytes currently used */
```

### Root Registration

```c
void habu_gc_add_root(void *ptr);    /* Register root */
void habu_gc_remove_root(void *ptr); /* Unregister root */
```

### Write Barrier

```c
void habu_write_barrier(void *obj, habu_value_t value);
```

**Note**: Automatically called by `habu_set_car`, `habu_set_cdr`, `habu_vector_set`.

### Allocation

```c
void *habu_gc_alloc(size_t bytes, uint64_t type);
habu_value_t habu_cons(habu_value_t car, habu_value_t cdr);
habu_value_t habu_make_vector(size_t length);
habu_value_t habu_make_string(const char *str, size_t length);
habu_value_t habu_make_symbol(const char *name);
```

## Implementation Files

- **runtime/gc.c**: Main GC implementation (800+ lines)
  - Copying collector for young generation
  - Mark-sweep-compact for old generation
  - Write barrier and remembered set
  - Root management
  - Allocation and statistics

- **runtime/habu.h**: Public API
- **runtime/object.h**: Object representation and helpers
- **runtime/runtime.c**: Accessor functions with write barriers

## Future Enhancements

### Incremental Collection

Currently, GC pauses are stop-the-world. Future work:
- Incremental marking (spread mark phase over multiple pauses)
- Concurrent sweeping (sweep old gen while mutator runs)

### Parallel Collection

Multi-core systems could benefit from:
- Parallel copying in young GC
- Parallel marking in old GC

### Remembered Set Optimization

Current remembered set is a simple array. Optimizations:
- Card table (bitmap of old gen regions with young pointers)
- Sequential store buffer (log recent stores, scan on GC)

### Adaptive Sizing

Static heap sizes (512 KB young, 4 MB old) could be adaptive:
- Grow/shrink based on allocation patterns
- Adjust GC threshold based on pause times

## Debugging

Enable GC logging (compile with `-DDEBUG_GC`):
```c
#define DEBUG_GC
```

This logs:
- GC triggers and pause times
- Objects copied/promoted
- Remembered set size
- Heap occupancy before/after GC
