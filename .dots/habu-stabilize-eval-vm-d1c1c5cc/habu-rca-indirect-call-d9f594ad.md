---
title: RCA indirect-call paths
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:16.848940+01:00\\\"\""
closed-at: "2026-02-20T01:00:30.939981+01:00"
close-reason: Fix JIT SSA constant dominance and coalesce liveness
---

src/interp/repl.zig: hoist JIT dispatch and src/interp/vm.zig call op paths. Cause: indirect-call/JIT path crashes under complex workloads. Fix: direct root-cause fix, no fallback masking.

## 2026-02-20 RCA + Fix

- Repro stabilized with JIT-only `nqueens-safe-p` mismatch (`(nqueens-safe-p 2 '(1) 1)` returned `t` under JIT, `nil` under interpreter).
- Hoist IR dump showed non-dominating constant use:
  - `block3: v9 = iconst 2`
  - `block6: v16 = isub v9, v14`
  - `block8: v22 = iadd v5, v9`
- Root cause in Habu translator: `preEmitConstants` did not recurse through `.block`, so TCO body constants were not pre-emitted from a dominating block.
- Fixes applied:
  - `src/jit/backend.zig`: `preEmitConstants` handles `.block`.
  - `src/jit/backend.zig`: `switchBlock` clears `const_cache` in `local_consts` mode to keep block-local constant scope safe.
  - `src/jit/backend.zig`: `coalesceMovs` now gates on CFG-aware `isRegDeadAfter` (no backedge heuristic shortcut).
- Regressions added:
  - `src/jit/backend.zig`: `coalesceMovs keeps source live across loop backedge`.
  - `src/jit/backend.zig`: `coalesceMovs folds dead copy in straight line`.
  - `src/tests/integration.zig`: `compileChunk JIT handles recursive nqueens helper entry copies`.
- Validation:
  - Targeted hoist/JIT regression filters all pass.
  - `zig build -Duse-hoist=true` passes.
  - REPL parity confirmed for `nqueens-safe-p` probes and `(nqueens 4)` now returns `2`.
