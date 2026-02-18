# Hoist API Delta Audit (2026-02-17)

## Scope
- Hoist commits:
  - `111c239c` Unify context compile API and callsites
  - `7871a6ef` Reduce egraph and codegen overhead
  - `eeea0c7c` Accelerate liveness operand collection
- Habu integration points:
  - `src/jit/backend.zig`
  - `src/interp/repl.zig`
  - `build.zig`, `build.zig.zon`

## Findings

### 1) Compile API remains source-compatible for Habu callsites
- Hoist still exports `context.ContextBuilder` and `Context.compileFunction`.
- Habu callsites in `src/jit/backend.zig` still compile with:
  - `ContextBuilder.init(...).targetNative().optLevel(...).callConv(...).verification(...).build()`
  - `ctx.compileFunction(&func)`
- Evidence:
  - Hoist `src/context.zig:73` (`compileFunction`)
  - Hoist `src/context.zig:175` (`ContextBuilder`)
  - Habu `src/jit/backend.zig:3197`, `src/jit/backend.zig:3216`, `src/jit/backend.zig:4960`, `src/jit/backend.zig:4964`

### 2) Liveness operand-collection API changed inside hoist backend implementation
- Hoist moved AArch64/S390X instruction operand collection to `getOperands(..., collector: anytype)`.
- This is internal to hoist regalloc and does not require Habu import changes.
- Evidence:
  - Hoist `src/backends/aarch64/inst.zig:2615`
  - Hoist `src/backends/s390x/inst.zig:313`
  - Hoist `src/regalloc/liveness.zig` fast-path collector usage

### 3) Recheck after latest hoist touch-up (2026-02-19)
- Upstream compile blocker was fixed in hoist and Habu now rebuilds and runs with hoist enabled.
- Verification gates run green in Habu:
  - `zig build`
  - `zig build -Duse-hoist=true`
  - `zig build test -Duse-hoist=true -Dtest-filter='hoist API contract probe'`
  - `zig build -Doptimize=ReleaseFast bench-comp -- --json` (ack path no longer stack-overflowing under JIT mode)
- Habu build defaults now enable hoist (`build.zig` `use-hoist` defaults to true), so JIT/perf gates no longer silently run against the stub backend.

### 4) Remaining issues are backend correctness/perf quality, not API breakage
- Indirect-call argument move ordering still depends on Habu post-pass repair (`fixCallArgMoves`) in `src/jit/backend.zig`.
- Entry parameter shuffle repair still depends on Habu post-pass (`fixEntryParamMovesAlloc`) in `src/jit/backend.zig`.
- These are architectural hoist backend gaps, not exported API mismatches.

## Current Habu-side action status

1. API adaptation: complete.
- No additional Habu import signature migration needed for current hoist revision.

2. Compile blocker dot: reverified.
- `habu-reverify-hoist-compile-b48554f1` is satisfied by the verification run above.

3. Follow-up (upstream quality work):
- Move parallel-copy resolution into hoist aarch64 lowering and delete Habu binary post-pass repairs.
- Keep perf rebaseline tied to upstream removal of these repairs.

## Notes for Dot Sequencing
- Keep `habu-audit-hoist-api-6ace8084` and `habu-adapt-habu-to-7e7240c7` closed.
- Mark `habu-fix-hoist-compile-9a100641` complete.
- Track call-indirect/entry-copy root-cause work under hoist migration dots, not API-delta dots.
