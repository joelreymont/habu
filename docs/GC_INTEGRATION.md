# Garbage Collection Integration

## Overview

Habu uses a mark-and-sweep garbage collector with automatic triggering when the heap is full. The GC is integrated with both the runtime and compiled code.

## GC Architecture

### Mark-and-Sweep Algorithm

1. **Mark Phase**: Starting from roots, recursively mark all reachable objects
2. **Sweep Phase**: Scan heap, free unmarked objects, compact memory

### Object Tags

Objects use 4-bit tags in their lower bits:
- `0x0`: Fixnum (no heap allocation)
- `0x1`: Cons cell
- `0x2`: Symbol  
- `0x3`: String
- `0x4`: Array

### Heap Structure

- **Header** (8 bytes): Size, tag, mark bit
- **Data**: Object-specific data
- **Alignment**: 16-byte aligned (allows 4-bit tags)

## Bootstrap Mode (Phase 1)

### GC Root Registry

In bootstrap mode, roots are managed via a global registry:

```lisp
(register-gc-root ptr)    ; Add root
(unregister-gc-root ptr)  ; Remove root  
(clear-gc-roots)          ; Clear all roots
```

**Automatic Triggering:**

When `heap-allocate` detects insufficient space:
1. Calls `(gc heap *gc-roots*)` with registered roots
2. Compacts heap
3. Retries allocation
4. Errors if still insufficient space

### Limitations

**Bootstrap mode GC has a critical limitation:**

Since compiled code calls runtime functions via FFI trampolines, intermediate values on the call stack are NOT automatically tracked as roots. This means:

❌ **Unsafe**: Allocating during complex expressions
```lisp
(car (cons (cons 1 2) (cons 3 4)))  ; Inner cons results might be GC'd!
```

✅ **Safe**: Simple allocations
```lisp
(cons 1 2)  ; Single allocation, no intermediate values
```

✅ **Safe with roots**: Register important values
```lisp
(let ((x (cons 1 2)))
  (register-gc-root x)
  ... more allocations ...
  (unregister-gc-root x))
```

**Why this happens:**

When compiled code calls `runtime-cons`, the car and cdr values are passed as arguments. If either argument itself allocated a cons cell, that newly allocated cell exists only in a register or stack slot during the FFI call. If GC triggers during the outer `cons` allocation, it won't see these intermediate values as roots.

### Workaround

For now, the heap is large enough (1MB) that GC rarely triggers during typical operations. Production code would need either:

1. **Conservative stack scanning** - treat all stack values as potential pointers
2. **Explicit root tracking** - compiler generates code to register/unregister roots
3. **Phase 2 (standalone)** - inline allocation eliminates this issue

## Standalone Mode (Phase 2 - Future)

In standalone mode with inline allocation:

- No FFI calls during allocation
- Compiler knows exact GC points
- Can generate precise root maps
- Stack/register scanning is straightforward

## GC Statistics

```lisp
(heap-stats)   ; Size, allocated, free, objects, utilization
(gc-stats)     ; GC count, time, average time
```

## Testing

Basic GC test:
```bash
sbcl --load bootstrap/test-gc.lisp
```

Current test coverage:
- ✅ Manual GC with empty roots (collects everything)
- ✅ Manual GC with registered roots (preserves reachable objects)
- ✅ Automatic triggering when heap fills
- ⚠️  Complex nested allocations (may fail in bootstrap mode)

## Performance

**Bootstrap mode:**
- GC time: ~0.001ms for small heaps
- Overhead: Minimal until heap fills
- Compaction: Always enabled (keeps heap compact)

**Heap size:**
- Default: 1MB (configurable)
- Max objects: ~32K cons cells (32 bytes each with headers)

## Future Work

- [ ] Conservative stack scanning for bootstrap safety
- [ ] GC root barriers in compiler-generated code  
- [ ] Generational GC for better performance
- [ ] Incremental GC to reduce pause times
- [ ] Parallel GC for multi-core systems
