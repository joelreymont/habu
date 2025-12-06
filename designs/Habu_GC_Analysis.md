# Habu GC Analysis and Improvement Suggestions

This document summarizes the current garbage collector implementation in Habu and suggests concrete improvements that fit the existing design.

It is based on the actual sources:

- `runtime/gc.c`
- `runtime/memory.lisp`
- `runtime/habu.h`, `runtime/object.h`
- `docs/runtime/GC_RUNTIME.md`
- `docs/runtime/GC_NATIVE.md`

---

## 1. What Habu’s GC actually is

### 1.1 High-level design

`runtime/gc.c` describes an **incremental generational garbage collector** with two generations:

1. **Young generation (512 KB)**  
   - Cheney-style copying collector.  
   - Two semispaces (`from-space` and `to-space`), only one active at a time.  
   - Collecting the young generation copies live objects from `from` to `to`, then swaps them.

2. **Old generation (4 MB)**  
   - Single contiguous region (size constant `OLD_GEN_SIZE`).  
   - Mark-sweep with optional compaction.  
   - Allocation is also bump-pointer into the old region.

The header comments state:

```c
/* Incremental Generational Garbage Collector
 *
 * This is a two-generation copying/mark-sweep GC based on the generational hypothesis:
 * "Most objects die young."
 */
```

There is also support for:

- A **gray stack** (for incremental or tri-color marking).
- A **remembered set** and write barrier to track old→young pointers.
- Basic GC statistics and thresholds.

### 1.2 Fixed-size heap and init()

The `init(size_t heap_size)` function in `gc.c` currently **ignores** its `heap_size` argument:

```c
/*
 * Parameters:
 *   heap_size - Requested heap size (currently ignored, uses fixed sizes)
 */
void init(size_t heap_size) {
    (void)heap_size;  /* Use fixed sizes for now */

    /* Allocate the main GC structure */
    gc_heap = malloc(sizeof(gc_heap_t));
    assert(gc_heap != NULL);

    /* Zero-initialize all fields */
    memset(gc_heap, 0, sizeof(gc_heap_t));
    ...
    gc_heap->gc_threshold = YOUNG_GEN_SIZE / 2;  /* Trigger GC after 256 KB allocated */
}
```

Constants:

- `YOUNG_GEN_SIZE` = 512 KB (per semispace, 1 MB total allocated for young gen).
- `OLD_GEN_SIZE`   = 4 MB.

This means:

- Total heap is effectively **fixed at ~5 MB** (plus overhead) regardless of the requested heap size.
- GC triggers when half the young gen is full (256 KB allocated) by default.

### 1.3 Fast-path allocation for generated code

A very important feature for the native compiler (and JIT) is the fast-path allocation exported from `gc.c`:

```c
/* FAST-PATH ALLOCATION POINTERS
 *
 * Export pointers to young_free and young_end so generated machine code
 * can perform inline bump-pointer allocation. These are pointers-to-pointers
 * so they remain valid even when the GC swaps semispaces.
 */
young_free_ptr = &gc_heap->young_free;
young_end_ptr  = &gc_heap->young_end;
```

The ARM64 code generator can then:

- Load `young_free` / `young_end` through these pointers.
- Perform a bump-pointer allocation inline.
- Fall back to calling into the runtime if `young_free` exceeds `young_end`.

This is **exactly what Habu’s JIT needs**: tight integration between GC and codegen via a stable ABI.

### 1.4 `runtime/memory.lisp`

`memory.lisp` provides a higher-level interface for the Habu runtime in Lisp:

- Defines a `heap` object (in Lisp terms) and accessors.
- Implements routines like `write-u64`, `read-u64`, etc., for manipulating raw memory.
- Initializes the runtime heap via:

  ```lisp
  (defun initialize-runtime (&key (heap-size (* 1024 1024)))
    "Initialize the runtime system with a heap"
    (setf *heap* (create-heap :size heap-size))
    (format t "Habu runtime initialized with ~D byte heap~%" heap-size))
  ```

This Lisp-level heap code is somewhat independent of, but conceptually mirrors, the C GC heap. As Habu becomes more self-hosting, the boundary between these two will tighten.

---

## 2. GC improvement suggestions, grounded in current implementation

### 2.1 Honor the `heap_size` parameter

Problem:

- `init(size_t heap_size)` ignores its `heap_size` argument and always allocates:
  - Young gen: `YOUNG_GEN_SIZE * 2` (1 MB total)
  - Old gen: `OLD_GEN_SIZE` (4 MB)

This is fine for early testing but can be a real limitation for:

- Larger programs or datasets.
- Stress testing, where you may want, say, a 256 MB heap.
- Embedded runs where you might want a much smaller footprint.

**Suggestion:**

- Interpret `heap_size` as the **total heap budget** for both generations and data structures.
- Derive `YOUNG_GEN_SIZE` and `OLD_GEN_SIZE` from it at init time, e.g.:

  ```c
  size_t young_bytes = heap_size / 5;      /* 20% of heap */
  size_t old_bytes   = heap_size - young_bytes;
  /* Ensure alignment and minimums */
  young_bytes = MAX(young_bytes, 256 * 1024);
  old_bytes   = MAX(old_bytes,  2 * 1024 * 1024);
  ```

- Replace `YOUNG_GEN_SIZE` and `OLD_GEN_SIZE` with configuration fields in `gc_heap_t`, computed at runtime.

This is a minimal change that:

- Makes Habu’s GC **configurable** without changing algorithms.
- Lets the native compiler choose an appropriate heap size depending on target system.

### 2.2 Tune GC thresholds and export them

Currently, `gc_heap->gc_threshold` is hard-coded as half of `YOUNG_GEN_SIZE`:

```c
gc_heap->gc_threshold = YOUNG_GEN_SIZE / 2;  /* Trigger GC after 256 KB allocated */
```

For a fixed heap, this is reasonable. For a dynamic heap:

- It may be useful to allow the threshold to be tuned:
  - Either via a runtime API,
  - Or via environment variables (e.g. `HABU_GC_YOUNG_THRESHOLD`).

**Suggestion:**

- Add a small API in C:

  ```c
  void gc_set_young_threshold(size_t bytes);
  ```

  that just sets `gc_heap->gc_threshold`.

- Add a Lisp-visible wrapper (e.g., in `runtime/memory.lisp`) to allow experimentation from the REPL.

This is very simple but will be invaluable for tuning behavior when the JIT starts creating lots of objects of varying lifetimes.

### 2.3 Make the write barrier as cheap as possible

`gc.c` implements a generational write barrier to track old→young references into the remembered set. The JIT’d code will exercise this barrier frequently for any mutation operations (e.g., `setf` of structure fields).

Checks:

- Ensure the write barrier is:

  - **Inlined** as much as possible (no function call per write).
  - **Branch-prediction-friendly** (e.g. fast-path “no young pointer” case falls through).

If any mutator paths currently call a C function like `gc_write_barrier(old_obj, new_obj)`, consider:

- Converting that to an `inline` function in `gc.c` or a `static inline` in a header file included by both runtime and any C helpers.
- For the ARM64 codegen, emit the barrier sequence directly in assembly if it’s simple enough (e.g. single comparison, branch, and a store into the remembered set).

Because you already **export fast-path allocation pointers** for inline bump allocation, doing the same for the barrier logic will keep JIT’d code tight.

### 2.4 Prepare for true incremental collection

The `gc.c` file names this “Incremental Generational GC” and contains:

- Fields like `gc_heap->state`.
- A gray stack and remembered set.
- Comments that suggest step-wise marking.

If those incremental features are not fully wired into the runtime yet (i.e., if collection still happens as full stop-the-world passes), consider the following **incremental plan**:

1. Keep the **current stop-the-world semantics** as the canonical implementation for now.
2. Introduce an internal API:

   ```c
   void gc_collect_young_step(size_t budget_bytes);
   void gc_collect_full_step(size_t budget_bytes);
   ```

   that performs only a *bounded* amount of GC work per call.

3. Have the runtime or VM (potentially even the ARM64 codegen via safepoints) call these functions periodically, e.g.:

   - At function returns,
   - On loop back-edges,
   - Or after N allocations.

This integrates naturally with the **JIT & safepoint story**:

- The compiler could be taught to insert “safepoint polls” in hot loops by emitting a quick call to a tiny runtime stub that may or may not do some GC work.
- For now, just calling the existing stop-the-world GC from safepoints is sufficient; later, those calls can be switched to `gc_collect_*_step` for true incremental behavior.

### 2.5 Better integration with native codegen

Since `gc.c` explicitly exposes `young_free_ptr` and `young_end_ptr` to generated code, the next natural integration step is:

- Have `arm64/codegen.lisp`:
  - Use these pointers for all `cons`/vector/closure allocations in the young generation.
  - Emit a **fast path** inlined bump-allocation and a **slow path** call into a runtime function (e.g., `gc_alloc_slowpath`) if the bump pointer reaches `young_end`.

If this is already in place, verify that:

- The slow path correctly:
  - Triggers a young-gen collection when necessary.
  - Promotes or copies survivors to old-gen as intended.
- Any **allocation of *large* objects** (e.g. big arrays) bypasses the young gen if the design calls for that (some GCs allocate large objects directly in the old generation to avoid copying them frequently).

This is less a change recommendation and more a reminder to **lean fully into the existing optimizations**: the GC has already been shaped with JIT (fast-path allocation) in mind; the compiler should make aggressive use of that.

---

## 3. GC + JIT: how they fit together in Habu

Given this GC design, the JIT architecture from the codegen document aligns naturally:

- **Inline allocation**: JIT’d code bumps `young_free` and only occasionally calls into GC.
- **Versioned functions**: specialized versions may have different allocation patterns (e.g., more stack vs. heap allocations) but all respect the same allocation ABI.
- **Safepoints**: can be modeled as simple calls or branches to a small C stub, which in turn may call `gc_collect_young()` or `gc_collect_full()` depending on thresholds and state.

Suggested immediate steps:

1. **Expose GC tuning knobs** (heap size, threshold) to the native compiler and runtime.
2. **Document** the allocation and safepoint ABI explicitly in `docs/runtime/GC_NATIVE.md` and a new file (see patch) like `docs/runtime/HABU_GC_IMPROVEMENTS.md`.
3. **Keep all incremental GC work behind a simple C API**, so that Habu’s ARM64 backend only needs to know:

   - How to bump-allocate from `young_free`.
   - How to call into `gc_alloc_slowpath()` when needed.
   - How to call `gc_poll()` at safepoints.

---

## 4. Patch summary

The accompanying patch is intentionally conservative:

- It **adds documentation** only, under:

  - `docs/runtime/HABU_GC_IMPROVEMENTS.md`

- It does **not** change any C or Lisp runtime code yet, but it lays out:

  - How to make `heap_size` meaningful.
  - How to expose thresholds.
  - How to structure future GC and JIT integration.

Once you are satisfied with the direction, follow-up patches can:

- Update `gc.c` to use dynamic sizes.
- Add the `gc_set_young_threshold` API.
- Start inserting explicit safepoint calls from the native compiler.

This approach minimizes risk to the existing, working GC while giving a clear roadmap for future improvements.
