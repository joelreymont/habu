# Stack Maps And GC Safepoints

This document specifies the *correct* design for running Habu's copying GC while executing JIT-compiled code, without building temporary root arrays.

Related:
- `docs/jit-abi.md` (current ARM64 JIT ABI)
- `docs/cranelift-parity.md` (parity matrix row: "Stack maps / GC safepoints")

## Requirements

- Precise root updates for a copying collector: every live `Value` root must be located and updated in place during GC.
- No pointers into GC-managed objects stored outside the GC heap unless they are re-derived from a GC root after each collection.
- Single dispatch point for GC enumeration: VM and JIT share one root-enumeration API.
- Zero allocations on the GC hot path (including JIT GC slow paths).
- A safepoint protocol that is enforceable by construction and verifiable.

## Terms

- **Root slot**: an address of a writable `Value` (`*Value`) that must be updated after moving GC.
- **Root range**: a contiguous slice of root slots (`[*]Value` + length).
- **Safepoint**: a program point at which execution may invoke GC and the runtime can enumerate all live roots.
- **Stack map**: metadata that maps a safepoint to the set of root slots (and optionally registers) that are live at that point.

## Current State (What Exists Today)

- VM GC uses explicit root buffers built on demand (stack/globals/frames copied into an `ArrayList(Value)`, then written back after GC).
- JIT runtime helpers (`src/jit/rt.zig`) collect roots by copying `frame_base..sp` and `const_pool[0..const_count]` into a temporary `ArrayList(Value)` and using `vm.ext_roots`.
- There is no safepoint metadata; "GC happens" only inside helper calls that catch `error.OutOfMemory` and invoke `vm.collectGarbage()`.

This works but is not Cranelift-class:
- It allocates on GC slow paths.
- It requires copying root data into/out of temporary buffers.
- It cannot scale to register allocation (live `Value`s in registers would not be updated).

## Design: Unified Root Enumeration API

The GC must be able to visit root slots without materializing a temporary array.

### RootSet Representation

Represent the full root set as two collections:

1) Contiguous ranges (fast path):
- Stack, globals, const pools, and other `[]Value` buffers.

2) Individual slots (irregular fields):
- `pending_throw_tag`, `pending_throw_value`, etc.
- Per-frame `closure` roots, per-handler pair fields, etc.

Proposed runtime types (names illustrative):

```zig
pub const RootRange = struct {
    ptr: [*]Value,
    len: usize,
};

pub const RootSet = struct {
    ranges: []const RootRange,
    slots: []const *Value,
};
```

GC entrypoint:

```zig
pub fn collectGarbage(self: *Heap, roots: RootSet) !usize;
```

Contract:
- `roots.ranges[i].ptr[0..len]` and `roots.slots[j].*` must remain valid and writable for the duration of collection.
- Roots must contain *only* real `Value` slots (no tagged pointers hidden in other types).
- The collector updates `Value`s in place.

### VM Integration

VM should build a `RootSet` directly from its in-memory structures:
- Ranges:
  - `stack[0..sp]`
  - `globals[0..num_globals]`
  - `secondary_values[0..secondary_values_count]`
  - `saved_chunks[0..saved_chunk_sp]` (as `Value` chunk-roots)
  - `ext_roots` (already `[]Value`)
- Slots:
  - scalar fields: pending throw/block fields, `current_package`, etc.
  - selected fields inside frame stacks where the storage is not already a `[]Value`.

Key invariant for correctness and performance:
- If VM needs a raw pointer for execution speed (e.g. `*Chunk`), it must have a corresponding GC root slot that is a `Value` and is updated by GC; the raw pointer is re-derived from that `Value` after GC.

This removes the "copy roots into ArrayList + write back" pattern from VM GC.

### JIT Integration (Non-negotiable Invariant)

At any point where JIT code can trigger GC, every live `Value` must be in a *writable memory slot* described by the current safepoint's stack map.

Registers are not writable root slots, so either:
- the JIT never holds `Value`s in registers across a safepoint, or
- it spills all live `Value`s to spill slots before invoking any helper that may GC.

## Design: Safepoint Protocol

### Safepoints

Safepoints exist at:
- any helper call that may allocate or may invoke `vm.collectGarbage()` (including OutOfMemory retry paths),
- optional backedges (for hot-loop detection / tiering),
- optional explicit poll points (future multi-threading).

The safepoint protocol is:
1) Ensure all live `Value`s are in stack-map-described slots (spill as needed).
2) Call helper / runtime entry.
3) If helper triggers GC, runtime enumerates roots using the active safepoint's stack map and updates slots.
4) Return to JIT; code reloads from updated slots as needed.

### How The Runtime Knows "Which Safepoint"

The runtime needs a stable way to locate the stack map for the current safepoint.

Cranelift-class approach:
- Keep a global `JitRegistry` in the VM that maps a return address (PC) to `CodeInfo`.
- Each `CodeInfo` has a `SafepointTable` mapping `pc_offset -> SafepointId`.
- Each `SafepointId` points to a `RootMap` (stack slot offsets, spill area layout).

Runtime obtains the return address from the helper call site (AArch64 LR / Zig `@returnAddress()`), looks up the `CodeInfo`, and then the `RootMap`.

### RootMap Encoding

Initial correct encoding (stack-slot-only):
- `frame_base`: a pointer to the base of the JIT value stack (or dedicated spill base).
- `slots`: `[]const u16` byte offsets from `frame_base` to `Value` slots.

Properties:
- Offsets are sorted and unique.
- Offsets are validated to be within the frame/spill area bounds.

Future extension (still compatible):
- Add register masks (callee-saved registers that hold spilled `Value`s) *only if* those registers are spilled to memory before GC.
- Add multiple bases (e.g. separate spill base vs VM value stack base) by encoding `(base_id, offset)` pairs.

### Constant Pools And Non-GC Pointers

Correctness rule:
- JIT metadata stored outside the GC heap (e.g. in `Jit` allocator memory) must not contain raw pointers into GC-managed objects.

Therefore:
- Do not keep `[*]Value` pointers into GC-managed `Chunk` constant pools inside `JitContext` across GC.
- Either:
  - keep constants in GC-managed memory and re-derive `const_pool` pointers from a GC root (`Value` to `Chunk`) after GC, or
  - keep constants in non-GC memory but register that memory as a root range for every collection (so the `Value`s inside are updated).

The second approach is acceptable only if the constant pool memory is owned by the runtime and always reachable (so it participates in GC as a root, not as untracked memory).

## Verification Strategy

- Static verification (build-time):
  - `SafepointTable` lookup correctness: PC in range -> safepoint exists.
  - RootMap offsets sorted, unique, in-bounds.
- Runtime assertions (debug builds only):
  - At GC entry from JIT: active safepoint must be set; frame_base must be non-null.
- Tests:
  - Unit tests for stack map encoding/decoding and lookup.
  - A regression test that runs JIT code which triggers GC and verifies all live values are preserved (requires integration + safepoints).

## Follow-up Work (Dots)

- Implement `RootSet`-based GC entrypoint and migrate VM GC enumeration to it.
- Add `JitRegistry` + `CodeInfo` + `SafepointTable`.
- Emit stack-map metadata while compiling bytecode/JIT IR.
- Teach runtime helper paths to enumerate roots using stack maps (no temporary arrays).
- Add JIT-vs-VM differential tests that include GC-triggering programs.

