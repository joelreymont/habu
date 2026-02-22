# Hoist Bug: VCode Successor Slice Corruption

## Symptom
- Panic during lowering:
  - `../hoist/src/machinst/vcode.zig:161`
  - `index out of bounds: index 2863311530 (0xAAAAAAAA)`
  - In `computePreds()` when reading `block.succs`.

## Deterministic Repro (outside Habu)
- Build a VCode with ~34+ blocks and one successor per block.
- `computePreds()` panics once successor storage grows past initial capacity.
- This reproduces with GPA, c allocator, and arena-backed allocators.

## Root Cause
- `finishBlock()` stores block successor slices into `self.succs.items`:
  - `const succs_slice = self.succs.items[succs_start..];`
- Later growth of `self.succs` triggers allocator remap/free paths.
- Old buffers get poisoned (`undefined` => `0xAA...`) by Zig allocator semantics.
- Previously stored `block.succs` slices still point to old poisoned buffers.
- Same structural risk exists for `params` in `startBlock()`.

## Upstream Hoist Fix (recommended)
In `../hoist/src/machinst/vcode.zig`:
1. Never store long-lived slices pointing into growable `ArrayList` buffers.
2. Either:
   - Store `(start,len)` indices into concatenated arrays and materialize slices on access, or
   - Allocate per-block owned successor/param slices (stable storage).
3. If keeping concatenated arrays + slices, slice must be exact bounded range:
   - `items[start .. start + len]` (not `items[start..]`), and backing storage must be guaranteed stable for function lifetime.

## Habu-side Mitigation
- Habu now uses a remap-stable allocator wrapper for Hoist compile allocations (`src/jit/backend.zig`) so old buffers remain readable during lowering.
- This avoids the crash without modifying `../hoist`, but upstream fix is still preferred.
