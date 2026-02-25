# PLAN — REVIEW-PLAN (Strict) Update

Last updated: 2026-02-25
Scope: adjudicate DR-001..DR-008 and convert accepted findings into executable plan items.

---

## 1) Findings Adjudication

| ID | Severity | Decision | Evidence (file:line) | Note |
|---|---|---|---|---|
| DR-001 | Critical | **Deferred pending HF-1 closure** | `src/jit/backend.zig:816-830`, `src/interp/vm.zig:2106-2109`, `src/interp/vm.zig:2636-2637`, `src/runtime/heap.zig:2898-2953` | Current evidence shows no VM GC inside JIT helper window; final adjudication is gated on HF-1 invariant decision and validation. |
| DR-002 | Critical | **Deferred pending HF-1 closure** | `src/jit/backend.zig:886-899`, `src/interp/vm.zig:2106-2109`, `src/interp/vm.zig:2636-2637`, `src/runtime/heap.zig:2204-2210` | Claim depends on GC during helper execution; in-JIT VM GC is forbidden, but helper-entry parity must be validated in HF-1 before closure. |
| DR-003 | Critical | **Deferred pending HF-1 closure** | `src/jit/backend.zig:902-917`, `src/interp/vm.zig:2106-2109`, `src/interp/vm.zig:2636-2637`, `src/runtime/heap.zig:2204-2210` | Same rationale as DR-002; keep deferred until HF-1 resolves/encodes the invariant with executable proof. |
| DR-004 | Major | **Accept** | `src/jit/backend.zig:2642-2655` | `translateVar` remap miss can fall through to unchecked `self.locals.items[v.index]`. |
| DR-005 | Major | **Accept** | `src/interp/repl.zig:3104-3110` | `known_fns.put(...) catch {}` masks allocator failures. |
| DR-006 | Major | **Accept** | `src/tests/integration.zig:9633-9670` | OOM relay test only checks final value; does not prove JIT-attempt + fallback path exercised. |
| DR-007 | Major | **Accept** | `src/jit/candidates.zig:62-71`, `src/jit/candidates.zig:160-182`, tests region `src/jit/candidates.zig:198-315` | Missing coverage for `set_symbol_function` collection and chunk signature/name matching. |
| DR-008 | Minor | **Accept** | `src/jit/candidates.zig:28`, tests region `src/jit/candidates.zig:198-315` | `stripQualifiedName` edge forms lack tests. |

---

## 2) Accepted Work Items

### P0-1. Remove allocator error masking in JIT known-function map setup (DR-005)
- **Files**: `src/interp/repl.zig:3104-3110` (and immediate call flow in `doHoistCompile`).
- **Change**:
  1. Remove `catch {}` from `known_fns.put(...)`.
  2. Convert to explicit failure path that returns `.failed` from `doHoistCompile` on allocator error.
- **Implementation steps**:
  1. In `doHoistCompile`, replace masked `known_fns.put(...) catch {}` with explicit error handling (`catch return .failed`).
  2. Keep map-build loop behavior deterministic: first allocation failure aborts compilation.
  3. Add a deterministic failing-allocator test harness (test-local allocator wrapper) that fails specifically during `known_fns.put`.
  4. Pre-seed `vm.jit_fns` with at least one compiled function so the `known_fns.put` path is guaranteed to execute.
  5. Assert compile attempt reports failure and `vm.jit_fns.items.len` is unchanged before/after attempt.
- **Acceptance criteria**:
  - No masked-error pattern remains in this block.
  - Add deterministic negative-path test in `src/interp/repl.zig` tests (or integration test helper) using a failing allocator wrapper that fails at `known_fns.put`.
  - Negative-path test asserts `doHoistCompile` returns `.failed` and no JIT registration side effects occur (`vm.jit_fns.items.len` unchanged across the failed compile attempt).
  - Existing JIT compile path tests continue to pass.
- **Edge cases to cover**:
  - Failure on first map insert (with pre-seeded `jit_fns`).
  - Failure after several successful inserts (partial population attempt).
  - Control case with no pre-seeded `jit_fns` (ensures no false-positive failure trigger).
  - Existing `jit_fns` entries remain callable after failed compile attempt.
- **Risk**: behavioral change from “best effort map fill” to deterministic failure; may reduce compile success under pressure (intended).
- **Estimate**: 20-40 min.
- **Dependencies**: none.

### P0-2. Harden `translateVar` remap bounds handling (DR-004)
- **Files**: `src/jit/backend.zig:2642-2655`, backend tests in same file test section.
- **Change**:
  1. Guard both remapped index and fallback index.
  2. On miss/out-of-range, return `error.UnsupportedIrNode` instead of unchecked indexing.
- **Implementation steps**:
  1. Replace raw `scope.base + v.index` with checked addition (`std.math.add`) and return `error.UnsupportedIrNode` on overflow.
  2. Add explicit bounds checks for remapped index and fallback `v.index` against `self.locals.items.len`.
  3. Keep current fast path for in-range accesses unchanged.
  4. Add regression tests for remap hit, remap miss with in-range fallback, remap miss/out-of-range fallback, and overflowed index math.
- **Acceptance criteria**:
  - No unchecked `self.locals.items[...]` access remains on remap miss path.
  - Overflow in remap index math is handled gracefully (`UnsupportedIrNode`), not panic/wraparound.
  - New regression tests cover inline-scope remap miss/out-of-range and overflow behavior.
- **Edge cases to cover**:
  - Empty `locals.items`.
  - Non-empty inline scope with `scope.base` near upper bound.
  - Large `v.index` that would overflow addition if unchecked.
- **Risk**: may reject previously accepted malformed IR; acceptable under hard-cutover correctness policy.
- **Estimate**: 30-60 min.
- **Dependencies**: none.

### P1-1. Strengthen OOM relay integration test to prove JIT attempt + fallback (DR-006)
- **Files**:
  - runtime signal: `src/interp/vm.zig` (`runJitCompiled` OOM fallback branch, `Vm` state/init/reset)
  - integration test: `src/tests/integration.zig:9633-9670`
- **Change**:
  1. Assert JIT compile attempt happened (e.g., `vm.jit_fns.items.len > 0` after optimized `defun`).
  2. Add deterministic pressure pattern (multi-call or larger workload) and assert GC activity delta during call phase.
  3. Add a direct fallback-taken signal for tests by incrementing a test-visible VM counter in `runJitCompiled` OOM fallback branch (`collectGarbage` + `return null` path).
  4. Assert post-pressure calls still return correct value (fallback path does not corrupt semantics).
- **Implementation steps**:
  1. Add `jit_fallback_oom_count: u64` to `Vm` state near existing JIT counters and initialize to `0` in `Vm.init`.
  2. Increment counter only in `runJitCompiled` when `err == error.OutOfMemory` and fallback returns `null`.
  3. Add reset helper (`resetJitFallbackOomCount`) and/or direct test access pattern consistent with existing `resetJitDirectCalls` style.
  4. Expand integration test to assert JIT-attempt (`jit_fns` growth), fallback signal increment, GC delta, and repeated stable semantics.
  5. Keep telemetry test-only in effect/usage (no production behavior changes).
- **Acceptance criteria**:
  - Capture `jit_before = vm.jit_fns.items.len` before `defun`; after `defun`, assert `vm.jit_fns.items.len > jit_before` (JIT attempt happened).
  - Capture `gc_before = heap.stats.gc_count` immediately before pressure calls; after pressure calls, assert `heap.stats.gc_count >= gc_before + 1`.
  - Assert direct fallback signal (`vm.jit_fallback_oom_count` or equivalent test hook) increments during the pressure run.
  - Run at least 3 repeated calls after first pressure-induced GC and assert identical result each time (fallback preserves semantics).
  - Add a short comment in-test describing fallback evidence contract (JIT attempt + fallback signal + GC delta + stable result).
- **Edge cases to cover**:
  - OOM occurs before any successful JIT return.
  - Multiple OOMs in one run (counter monotonicity).
  - No-OOM control case keeps fallback counter unchanged.
- **Risk**: tiny-heap OOM tests can be timing-sensitive/flaky.
- **Estimate**: 60-120 min.
- **Dependencies**: none.

### P1-2. Fill candidates matching/collection coverage gaps (DR-007)
- **Files**: `src/jit/candidates.zig`:
  - logic: `:62-71` (`set_symbol_function` path), `:160-182` (`findMatchingChunk`)
  - tests: `:198-315` (extend).
- **Change**:
  1. Add tests for `.set_symbol_function` candidate collection, including quote-symbol and fallback-name behavior.
  2. Add direct `findMatchingChunk` tests for signature match/mismatch (arity/opt/key/rest).
  3. Add direct name-match tests for symbol vs string chunk names and qualified/local candidate names.
  4. Add `used[]` mutation/exclusion assertions.
  5. Add len-mismatch coverage (`child_chunks.len != used.len`) and case-variant name matching expectations.
  6. Harden matcher for non-chunk `child_chunks` entries (`isChunk` guard before `toPtr`) and add negative tests.
- **Implementation steps**:
  1. Add lightweight chunk fixture helper in candidates tests (heap + minimal chunk allocation) so signature/name paths are exercised against real `Chunk` values.
  2. Add one test per matching dimension: arity, optional count, key count, rest flag.
  3. Add name tests for symbol-name exact, string-name exact, qualified/local variants, and case-insensitive comparisons.
  4. Add `used[]` mutation tests: mutation on success, no mutation on mismatch.
  5. Add explicit `.set_symbol_function` coverage for quote-symbol left operand and fallback-name branch.
  6. Add one adjacent integration smoke using `src/testing/compile_chunk.zig` call path to ensure matcher assumptions hold in real hoist flow.
- **Acceptance criteria**:
  - Each behavior above has at least one dedicated test.
  - `used[]` changes only on successful match.
  - `findMatchingChunk` returns null when `child_chunks.len != used.len`.
  - Matcher handles non-chunk `child_chunks` entries safely (guard or explicit assertion contract) with dedicated negative test coverage.
  - Existing candidate tests remain green.
- **Edge cases to cover**:
  - Empty `child_chunks`.
  - All chunks pre-marked `used`.
  - Mixed symbol/string chunk names in same candidate scan.
  - Non-chunk value in `child_chunks` must not panic.
- **Risk**: constructing realistic chunk fixtures in unit tests may be verbose.
- **Estimate**: 45-90 min.
- **Dependencies**: none.

### P2-1. Add `stripQualifiedName` edge-case tests (DR-008)
- **Files**: `src/jit/candidates.zig:28`, tests region `:198-315`.
- **Change**:
  - Add explicit edge-case tests for forms like `"PKG:NAME"`, `"PKG::NAME"`, `"NAME"`, `"::NAME"`, `"PKG::"`.
- **Acceptance criteria**:
  - Expected output is asserted for each edge form.
  - Behavior remains documented by tests only (no semantics change unless tests expose a bug).
- **Risk**: clarifies potentially ambiguous legacy input behavior.
- **Estimate**: 15-30 min.
- **Dependencies**: P1-2 (same test file; batch together).

### P0-3. Resolve `jitResolveForwarded` parity decision for hash helpers (HF-1)
- **Files**:
  - helper logic: `src/jit/backend.zig:816-917` (`jitHashSet`, `jitHashKeys`, `jitHashAlist`)
  - VM fallback paths: `src/interp/vm.zig` (`tryCallJit`, `tryDirectCallJit`, `runJitCompiled` OOM->null path)
- **Change**:
  1. Decide and document invariant: either all hash helpers call `jitResolveForwarded` at entry, or current asymmetry is intentional.
  2. If intentional, add in-code comment near helpers citing no-GC execution fence assumptions and why asymmetry is safe.
  3. If not intentional, patch `jitHashKeys`/`jitHashAlist` to mirror `jitHashSet` forwarded-resolution behavior and add regression coverage.
- **Implementation steps**:
  1. Make invariant decision explicitly in code comments (not commit-message-only).
  2. Add executable proof:
     - helper-side test/proof for chosen invariant,
     - VM-side proof that both `tryCallJit` and `tryDirectCallJit` preserve OOM->`null` fallback semantics after helper changes.
  3. Add path observability assertions in tests:
     - force generic JIT entry path and assert `jit_direct_calls` remains unchanged,
     - force direct JIT entry path and assert `jit_direct_calls` increments.
  4. Re-run hash helper behavior checks (set/keys/alist) under JIT-compiled call sites.
- **Acceptance criteria**:
  - Plan/code explicitly states chosen invariant with file-local comments.
  - No ambiguity remains between DR-001..003 deferred status and open-risk notes.
  - Add executable proof: either a regression test for helper-entry behavior or a runtime/assert invariant check in debug/test builds.
  - Proof must cover both VM JIT entry paths (`tryCallJit` and `tryDirectCallJit`) and verify OOM/`null` fallback contract remains coherent.
  - Path observability is explicit in tests (generic path with unchanged `jit_direct_calls`, direct path with incremented `jit_direct_calls`).
  - HF-1 is considered complete only when DR-001..003 are explicitly re-adjudicated from deferred -> resolved in this plan with evidence links.
  - Document chosen proof mechanism in commit message and LESSONS entry.
- **Edge cases to cover**:
  - Forwarded-table input value at helper entry.
  - Non-hash table input passed to helper paths.
  - OOM during helper activity still triggers VM fallback coherently.
- **Risk**: over-hardening may add unnecessary loads on hot JIT helper paths.
- **Estimate**: 30-90 min.
- **Dependencies**: none.

---

## 3) Deferred Critical Findings (Pending HF-1)

1. **DR-001 deferred**: stale `ht` across grow/GC is not reproducible under current JIT no-GC execution model, but final closure depends on HF-1 invariant/proof.
   - JIT execution increments `jit_gc_forbidden_depth` before bridge call and decrements after (`src/interp/vm.zig:2106-2109`).
   - `collectGarbageExtra` hard-fails under this fence (`src/interp/vm.zig:2636-2637`).
   - `growHashTableInPlace` does not invoke VM GC (`src/runtime/heap.zig:2898-2953`).
2. **DR-002 deferred**: stale pointer via `allocCons/GC` in `jitHashKeys` depends on in-JIT GC, which is blocked; helper-entry parity/invariant still must be encoded and validated by HF-1.
3. **DR-003 deferred**: same rationale as DR-002 for `jitHashAlist`; close only after HF-1 proof mechanism passes.

---

## 4) Priority + Execution Order

1. **P0-1 (DR-005)** — eliminate masked errors first.
2. **P0-2 (DR-004)** — remove unchecked index fallthrough.
3. **P0-3 (HF-1)** — resolve/encode hash-helper forward-resolution invariant before closing critical-origin items.
4. **P1-1 (DR-006)** — harden integration test with explicit measurable fallback evidence.
5. **P1-2 (DR-007)** — close candidate/matching coverage gaps.
6. **P2-1 (DR-008)** — add strip-qualified edge tests while already touching candidates test block.

Batch validation target after P0/P1 batch: `zig build test`.

---

## 5) Gap-Fill Execution Plan + Review Gates

### Batch A — P0-1 + P0-2 (error masking + bounds hardening)
- **Implementation**: `src/interp/repl.zig` (`doHoistCompile` map-fill path), `src/jit/backend.zig` (`translateVar`).
- **Tests**:
  - deterministic failing-allocator negative path for `known_fns.put`.
  - remap miss/out-of-range regression tests for `translateVar`.
- **Validation**: run focused test targets for REPL/JIT backend tests, then include in batch-wide test pass.
- **Review gate (required)**: run parallel `plan-critic + edge-case-hunter` focused on changed sections; no new unresolved Critical findings in Batch-A scope and no unresolved Major findings in touched files before continuing.

### Batch B — P0-3 (HF-1 critical closure)
- **Implementation**: `src/jit/backend.zig` hash-helper invariant decision/proof + VM entry-path coherence checks in `src/interp/vm.zig`.
- **Tests**:
  - helper invariant proof (regression or debug/test assert).
  - explicit coverage for both `tryCallJit` and `tryDirectCallJit` fallback coherence.
- **Validation**: run focused JIT helper + VM tests.
- **Review gate (required)**: full parallel review-plan mini-pass (plan-critic x2 + edge-case-hunter + scout) for HF-1 closure criteria; DR-001..003 remain deferred unless this gate explicitly passes.

### Batch C — P1-1 (OOM fallback observability)
- **Implementation**: add `jit_fallback_oom_count` (or equivalent) in `Vm`, increment in `runJitCompiled` OOM->fallback branch, extend integration test.
- **Tests**:
  - JIT-attempt assertion,
  - fallback counter increment assertion,
  - GC delta assertion,
  - repeated semantic stability.
- **Validation**: run OOM integration test 3 consecutive times (determinism gate).
- **Review gate (required)**: parallel `plan-critic + edge-case-hunter` focused on observability/test-flake risks.

### Batch D — P1-2 + P2-1 (candidate + strip-qualified coverage)
- **Implementation**: extend `src/jit/candidates.zig` tests with chunk fixtures and matching edge cases.
- **Tests**:
  - `set_symbol_function` collection,
  - signature/name/used[]/len-mismatch behavior,
  - strip-qualified edge forms.
- **Validation**: run candidates tests plus batch-wide suite.
- **Review gate (required)**: parallel `plan-critic + edge-case-hunter` focused on missing coverage edges.

### Batch completion policy
- After completing Batches A-D, run `zig build test` once for the full batch.
- If environment blocks full run, document blocker + run focused substitutes for each batch section.
- Do not close DR-001..003 until Batch B gate passes.

## 6) Open Risks / Unknowns

- DR-001..003 remain **deferred** until HF-1 invariant/proof closes; do not mark these critical-origin items complete before **P0-3 (HF-1)**.
- DR-006 closure requires direct fallback telemetry assertion in addition to proxy signals (`vm.jit_fns` growth, `heap.stats.gc_count` delta, repeated semantic stability); telemetry hook location is the OOM fallback branch in `runJitCompiled`.
- Determinism policy: run the DR-006 test 3 consecutive local reruns; if any flake remains after adding fallback telemetry, gate closure on tightening workload/heap thresholds until all 3 pass.
