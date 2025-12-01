# Native ARM64 Garbage Collector

## Overview

Habu implements a pure ARM64 garbage collector for native binaries. This design
enables future incremental/real-time GC while keeping the system fully self-hosted.

## Why Pure ARM64?

1. **Self-hosting purity** - No C runtime dependency
2. **Incremental-ready** - Full control over pause points
3. **Write barrier control** - Emit barriers directly in codegen
4. **No ABI friction** - Native register conventions
5. **Memory-bound performance** - GC speed limited by RAM, not CPU

## Architecture

### Phase 1: Stop-the-World Copying Collector (Current)

Simple Cheney's algorithm with two semispaces:

```
Heap Layout:
┌────────────────────────────────────────────────────────┐
│ Globals (48 bytes)                                     │
│   [0]:  intern_table    (tagged pointer)               │
│   [8]:  lambda_counter  (untagged integer)             │
│   [16]: from_end        (GC trigger address)           │
│   [24]: half_heap_size  (constant)                     │
│   [32]: space_flag      (0 or half_heap_size)          │
│   [40]: gc_state        (for incremental GC)           │
├────────────────────────────────────────────────────────┤
│ Semispace 0: [x27+48 .. x27+48+half)                   │
├────────────────────────────────────────────────────────┤
│ Semispace 1: [x27+48+half .. x27+48+2*half)            │
└────────────────────────────────────────────────────────┘
```

### Phase 2: Incremental Tri-Color Marking (Future)

Upgrade path to near-real-time GC:

```
GC States:
  0 = IDLE       - Normal allocation
  1 = MARKING    - Incrementally marking reachable objects
  2 = COPYING    - Evacuating live objects to to-space
  3 = FLIPPING   - Atomic space flip

Tri-Color Invariant:
  WHITE = Not yet seen (potential garbage)
  GRAY  = Seen, children not processed (in worklist)
  BLACK = Fully processed (definitely live)

Write Barrier (Dijkstra):
  On store of ptr into obj:
    if obj is BLACK and ptr is WHITE:
      mark ptr GRAY (add to worklist)
```

## Object Layout

No headers - type determined by tag bits:

```
Tag  Type     Size Calculation
───  ────     ────────────────
0    Fixnum   Not a heap object (immediate)
1    Cons     16 bytes (car + cdr)
2    Symbol   8 bytes (pointer to name string)
3    Vector   8 + length*8 bytes
4    String   8 + align16(length) bytes
5    Closure  16 bytes (fn-offset + env)
6    Nil      Not a heap object (immediate 0x06)
7    Forward  Forwarding pointer (GC only)
```

## Register Usage

```
x27 = Heap base (globals start here)
x28 = Allocation pointer (bump pointer)
x25 = (Future) Gray stack pointer for incremental GC

Temporaries during GC:
x16 = to_scan (Cheney scan pointer)
x17 = to_free (allocation in to-space)
x18 = from_start
x19 = from_end
```

## GC Trigger

After every allocation:

```asm
gc_check:
    ldr x9, [x27, #16]     ; from_end
    cmp x28, x9
    b.lo continue
    bl gc_collect
continue:
```

Cost: 4 instructions per allocation (~1-2 cycles if branch predicted).

## Copying Algorithm

### gc_collect

```
1. Save registers x0-x7, x24 (roots)
2. Setup to-space:
   - to_scan = to_free = to_start
3. Copy roots:
   - intern_table at [x27+0]
   - saved x0-x7 (function args/return)
   - saved x24 (closure environment)
4. Cheney scan loop:
   while to_scan < to_free:
     for each word at to_scan:
       if is_heap_pointer(word) and in_from_space(word):
         word = copy(word)
     to_scan += 8
5. Flip spaces:
   - space_flag = half - space_flag
   - from_end = new boundary
   - x28 = to_free
6. Restore registers
```

### copy(ptr)

```
1. Check tag - if 0 (fixnum) or 6 (nil), return unchanged
2. Get base address (ptr & ~0xF)
3. Check if in from-space - if not, return unchanged
4. Load first word - check if tag 7 (forwarded)
   - If forwarded, return forward_addr | original_tag
5. Calculate object size from tag
6. Copy bytes from base to to_free
7. Install forwarding pointer: base[0] = to_free | 7
8. Advance to_free by object size
9. Return to_free | original_tag
```

## Write Barriers (Future Incremental GC)

For incremental GC, we need write barriers on pointer stores:

```asm
; Before: (setcar cons new-value)
; After:
write_barrier:
    ; Check if cons is in to-space (BLACK)
    ; Check if new-value is in from-space (WHITE)
    ; If both true, mark new-value GRAY
    ldr x9, [x27, #40]     ; gc_state
    cbz x9, no_barrier     ; skip if IDLE
    ; ... barrier logic ...
no_barrier:
    str x0, [x1, #0]       ; actual store
```

Barrier cost: ~3 instructions when IDLE (branch predicted), ~10 when active.

## Performance Analysis

### Memory-Bound Reality

GC is dominated by memory bandwidth, not instruction count:

```
For 50MB live data, ~1.5M objects:
- Memory read:  50MB @ 50GB/s = 1ms
- Memory write: 50MB @ 50GB/s = 1ms
- Scanning:     1.5M * 10 inst @ 3GHz = 5ms
- Total:        ~7-10ms pause

With incremental (1000 objects/quantum):
- Per quantum: ~0.1ms
- Mutator utilization: >95%
```

### Why ARM64 Is Fine

Our codegen produces working but not optimal code. For GC this doesn't matter:

1. **Copy loop is tight** - ldp/stp pairs, 16 bytes per iteration
2. **Branch prediction works** - same pattern every time
3. **Memory is the bottleneck** - CPU waits on RAM anyway
4. **Small code** - fits in L1 icache

## Implementation Approach

The GC is written as **Habu Lisp functions** that compile to native ARM64:

```lisp
;; gc-runtime.lisp - Included in every native binary

(defun gc-copy (ptr)
  "Copy object to to-space, return new address with original tag."
  (let ((tag (logand ptr #xF)))
    (if (or (= tag 0) (= tag 6))  ; fixnum or nil
        ptr
        (let ((base (logand ptr (lognot #xF))))
          (if (gc-in-from-space base)
              (let ((first-word (mem-load base)))
                (if (= (logand first-word #xF) 7)  ; forwarded?
                    (logior (logand first-word (lognot #xF)) tag)
                    (gc-do-copy base tag)))
              ptr)))))

(defun gc-collect ()
  "Stop-the-world collection."
  ;; ... save roots, copy, scan, flip ...
  )
```

This is truly self-hosted: the GC is Habu code compiled by Habu.

### Files

- `bootstrap/gc-runtime.lisp` - GC functions (Habu source)
- `bootstrap/macho.lisp` - Heap layout initialization
- `bootstrap/codegen.lisp` - GC trigger insertion after allocations

## Roadmap

### Phase 1: Stop-the-World (Now)
- [x] Design heap layout
- [ ] Implement gc_collect function
- [ ] Implement copy function
- [ ] Add triggers after allocations
- [ ] Test with self-compilation

### Phase 2: Incremental Marking
- [ ] Add gc_state global
- [ ] Implement write barriers
- [ ] Gray stack management
- [ ] Incremental mark loop
- [ ] Bounded pause times

### Phase 3: Concurrent Collection
- [ ] Background marking thread
- [ ] Read barriers for to-space forwarding
- [ ] Concurrent sweep
- [ ] Sub-millisecond pauses

## Comparison with C Runtime

The existing `runtime/gc.c` is a well-tested generational GC but:

1. **Different object layout** - uses 16-byte headers
2. **Requires C library** - malloc, memmove, etc.
3. **Not self-hosted** - depends on external compiler

The pure ARM64 approach:
- Matches native Habu object format
- Fully self-contained
- Sets up incremental GC path
- Educational value

## Testing Strategy

1. **Unit tests** - Small programs that allocate and discard
2. **Stress tests** - Fill heap multiple times
3. **Self-compilation** - The ultimate test (256KB source)
4. **Correctness** - Compare output before/after GC

## References

- Cheney, C.J. "A Nonrecursive List Compacting Algorithm" (1970)
- Wilson, P. "Uniprocessor Garbage Collection Techniques" (1992)
- Jones, Hosking, Moss. "The Garbage Collection Handbook" (2011)
