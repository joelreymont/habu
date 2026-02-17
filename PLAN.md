# Master Plan (Dot-Driven)

## Contract
- Every executable plan leaf is a dot.
- Plan checkboxes map 1:1 to dot IDs.
- A task is complete only when its dot is closed.
- Work order is top-to-bottom unless an explicit dependency says otherwise.

## Tree

### 0. Plan Control
- [ ] `habu-unify-plan-and-1848633e` Unify plan and dot tree.
- [ ] `habu-run-full-ansi-a5719d99` Run full ANSI baseline and refresh machine-readable results.
- [x] `habu-hoist-cleanup-gate-2b9f46d0` Hoist migration cleanup gate before resuming Maxima active work.
  - [x] `habu-audit-legacy-backend-f3c3848f` Audit legacy backend references and anti-patterns.
  - [x] `habu-drop-dead-ir-27996ee9` Drop dead legacy IR backend export/module.
  - [x] `habu-scrub-stale-backend-d8b2bb66` Scrub stale backend docs and invalid file references.
  - [x] `habu-verify-hoist-only-4707566f` Verify hoist-only live paths via grep/build.
  - [x] `habu-perf-audit-2x-16c402b2` Performance audit and 2x plan.

### 1. Reader/Parser
- [ ] `habu-add-reader-support-bf089de4` Add reader `#.` support.
- [ ] `habu-fix-na-terminal-75305000` Fix `#nA` terminal element parsing.

### 1B. Performance 2x
- [ ] `habu-2x-perf-exec-68f37b3e` Execute 2x performance plan.
  - [x] `habu-audit-hoist-api-6ace8084` Audit hoist API delta.
  - [x] `habu-adapt-habu-to-7e7240c7` Adapt Habu to hoist API changes.
  - [x] `habu-rewire-jit-eligibility-699cbe9e` Rewire JIT eligibility after API sync.
  - [x] `habu-rebaseline-perf-post-b340b0e2` Rebaseline perf after hoist sync.
  - [x] `habu-fix-bench-comp-4a26be60` Fix comprehensive benchmark JIT crash (gcd path).
  - [x] `habu-raise-jit-coverage-51d21fa9` Raise JIT coverage for current interpreter-only workloads.
    - [x] `habu-hash-insert-bench-fcce9fed` JIT `make-hash-table`/`setf gethash`/`hash-table-count` benchmark path.
    - [x] `habu-hash-lookup-bench-65e5589f` JIT `gethash` lookup path with hash growth-safe set.
    - [x] `habu-str-search-bench-ec385e1b` JIT `make-string`/`setf char`/`position` benchmark path.
    - [x] `habu-gc-vector-bench-b4995d1c` JIT `make-array`/`aref` benchmark path.
    - [ ] `habu-mapcar-bench-jit-622b58d5` Add lambda/closure lowering for HOF benchmark calls.
    - [ ] `habu-reduce-bench-jit-b7c95d90` Add reduce HOF JIT path on top of closure lowering.
    - [ ] `habu-sort-fixnum-bench-f2e5e01d` Add sort comparator lowering for function designators.
    - [ ] `habu-sort-str-bench-a3f88f51` Add string sort path (`symbol_function`/comparator dispatch).
    - [ ] `habu-jit-float-support-91148537` Resolve float call-target and boxed-float lowering.
    - [ ] `habu-str-concat-bench-c576d53b` Resolve `concatenate` call-target path for string concat.
    - [ ] `habu-intern-bench-jit-1a268ee9` Finish intern benchmark JIT wiring end-to-end.
  - [x] `habu-cut-gc-root-25d3bb03` Cut GC root-set assembly overhead in VM collection path.
  - [x] `habu-fix-hoist-compile-9a100641` Fix hoist dependency compile blocker.

### 2. Compiler Core
- [ ] `habu-fix-loop-macro-c7a41441` Fix LOOP macro dispatch.
- [ ] `habu-support-loop-for-6e9d9623` Support LOOP `for ... and ...` clauses. Depends on `habu-fix-loop-macro-c7a41441`.
- [ ] `habu-support-setf-bit-b72546e8` Support `(setf (bit/sbit ...))` places.
- [ ] `habu-support-setf-generic-67036246` Support generic names `(setf foo)` in DEFGENERIC/DEFMETHOD.
- [ ] `habu-fix-setf-invalidsyntax-2e7560f2` Fix remaining `setf` InvalidSyntax in misc type-prop. Depends on `habu-signal-symbol-pkg-e766fbcf`.
- [ ] `habu-fix-concatenate-compiler-1e8d411f` Fix concatenate compiler fast-path semantics.
- [ ] `habu-fix-log-optional-de674bd9` Fix LOG optional base lowering. Depends on `habu-fix-concatenate-compiler-1e8d411f`.
- [ ] `habu-fix-defstruct-keyword-fe214c20` Fix DEFSTRUCT keyword/`:conc-name` parsing.
- [ ] `habu-fix-defstruct-invalid-85b8fcf9` Fix remaining DEFSTRUCT InvalidSyntax paths.
- [ ] `habu-fix-defstruct-copier-952e241d` Fix DEFSTRUCT copier fallback generation.

### 3. Runtime / Package / Stream / Time
- [ ] `habu-fix-pathname-merge-71b041a8` Fix pathname merge for compile-file-pathname.
- [ ] `habu-guard-core-pkg-a8f23f9b` Guard deletion of core packages.
- [ ] `habu-fix-finish-output-15f73282` Fix finish-output/force-output stream designator behavior.
- [ ] `habu-add-encode-universal-fe1b93d9` Add encode-universal-time primitive + wiring.
- [ ] `habu-signal-symbol-pkg-e766fbcf` Signal SYMBOL-PACKAGE type errors as Lisp conditions.
- [ ] `habu-fix-symbol-fn-1df6e2c3` Fix symbol-function primitive fallback/wrapper resolution.

### 4. VM / GC / Eval / CLOS / Conditions
- [ ] `habu-fix-gc-chunk-7057f649` Fix GC chunk root corruption.
- [ ] `habu-fix-transitive-lambda-f02bd0d9` Fix transitive lambda capture lowering.
- [ ] `habu-fix-nested-eval-b0bbd02d` Fix nested eval non-local exits. Depends on `habu-fix-gc-chunk-7057f649`.
- [ ] `habu-fix-clos-superclass-2aa44685` Fix CLOS superclass alias resolution.
- [ ] `habu-fix-warn-apply-fe791fc7` Fix warn/apply nil callee path.
- [ ] `habu-fix-ansi-deftest-faa1296f` Fix ANSI DEFTEST TypeMismatch root cause.
  - [ ] `habu-trace-first-ansi-b16c4bd5` Trace first uncaught ANSI TypeMismatch.
  - [ ] `habu-patch-typemismatch-root-b693b071` Patch root cause.
  - [ ] `habu-add-regression-for-1c3dfe97` Add focused regression.
  - [ ] `habu-verify-ansi-progression-d3af8a76` Verify ANSI progression and update baseline artifacts.

### 5. Maxima Continuation
- [x] `habu-increase-default-heap-44a06bce` Increase default heap and build comprehensive Maxima loader.
- [ ] `habu-maxima-end-to-efe58661` Maxima end-to-end integration test. Depends on `habu-cut-gc-root-25d3bb03` and `habu-fix-hoist-compile-9a100641`.
  - [x] `habu-maxima-core-loader-999c7eb3` Add Maxima core subset loader + entrypoint binding integration gate.
- [ ] `habu-fix-maxima-cas-a491af14` Fix Maxima CAS operations: integrate, solve, factor, limit, ratsimp, det. Depends on `habu-maxima-end-to-efe58661`.

## Execution Loop
1. Pick the first unblocked unchecked leaf.
2. `dot on <id>`.
3. Implement + test.
4. `dot off <id> -r "completed"`.
5. Check the leaf in this file.
6. Repeat until all leaves are checked.

## Done Criteria
- All leaves above checked.
- No open/active dots for IDs listed in this file.
- `tools/ansi/run.sh habu` produces updated baseline artifacts in `docs/ansi/results/`.
