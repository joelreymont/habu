# Restart

Habu is a self-hosted native ARM64 Forth with a stack-effect checker. This
note orients a restart on the current line. For build/recovery mechanics see
docs/bootstrap.md, for native debugging see docs/debugging.md, and for
durable technical findings see LESSONS.md (entries are dated).

## Where things stand (2026-09-16)

The engine protects its three VM stacks (data, return, loop) with guard
pages instead of per-transfer bounds checks. STACK-GUARD:EMIT-MAP
(src/habu/rt.f) maps each stack on its own page-aligned extent with an
inaccessible page on both sides; a fault is named by src/habu/crash.f as
`hb: stack bounds exceeded (data|return|loop)`, exit 102. run-in-stack admits
only a mapping made by lib/memory.f MEM-ALLOC-GUARDED and throws
E-STACK-UNGUARDED otherwise. The return and loop stacks live outside the DATA
header, based at STACK-ABI:RETURN-BASE-CELL and LOOP-BASE-CELL. The checker
still makes underflow in compiled code impossible; the interpreter keeps its
own depth floor.

Cedar's 2026-09-14 per-transfer guard series (a check on every transfer, call
and JIT push) is gone: it cost 5.2 of 7.4 MB of engine code and 213 s of
prefix compile time to solve what was really a capacity problem, not a
per-access one. Removed 2026-09-15, Joel's decision; see LESSONS.md.

Cedar's workspaces and handoff are gone; hazel owns this line. No bookmark
tracks the current tip (cedar/compiler-integration is real but several
commits stale). Work proceeds in small `.jj-ws/hazel-*` workspaces, one per
dot, landing in sequence onto the chain this workspace's parent sits on.

## Build and gate

- Rebuild: `bin/hb --load tools/native-build.f -- <out>`, about 138 s on this
  machine. Two builds from the same directory reach a byte fixpoint; two
  builds from different directories differ only in baked source paths
  (dot habu-bake-prefix-src-1047b604, open).
- Full gate: `bin/hb --load test/run.f`, about 15 minutes.
- A trivial program starts in about 20-50 ms; most of that is still AOT call
  sites resolved by name at boot (dot habu-bind-baked-call-e4d5b58f, open,
  targets under 20 ms).
- The engine is about 6.2 MB (6,226,112 bytes before the in-flight
  immediate-fold shrink below).
- The stripped AOT DATA image is sparse (dot
  habu-store-the-snapshot-203a86a0).

If `bin/hb` is missing or stale, follow docs/bootstrap.md. For a native
crash, follow docs/debugging.md.

## In flight

- habu-replace-per-transfer-8523fb98 (active): the guard-page rewrite
  itself, landing in slices; most recent is "Re-pin the return-stack
  lowering for guard pages".
- habu-fold-transfer-immediates-f043cef4 (active, .jj-ws/hazel-fold-imm):
  restore the pre-guard immediate folding the guard series displaced.
- 2026-09-16 profiling of the self-build (138.9 s, not syscall-bound) found
  two compiler defects: dead reload/store-back pairs in tier 1
  (habu-elide-same-slot-443de377) and uninlined trivial primitives
  (habu-inline-trivial-engine-922133ca); plus follow-on work to keep
  measuring and building this: the internal profiler
  (habu-build-the-internal-4cd07a82), tree-relative baked source names
  (habu-bake-prefix-src-1047b604), host-independent build facts
  (habu-derive-baked-build-d5df4419), sparse snapshots
  (habu-store-the-snapshot-203a86a0), and the gate's wall clock
  (habu-cut-the-gate-3fd6352e).

Run `dot ls` for the current open list; there are well over a thousand dots
tracked in this repository, most unrelated to this line.
