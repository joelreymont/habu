# Master Plan (Dot-Driven)

## Contract
- Every executable plan leaf is a dot.
- Plan checkboxes map 1:1 to dot IDs.
- A task is complete only when its dot is closed.
- Work order is top-to-bottom unless an explicit dependency says otherwise.

## Maxima Fast Targets
- Correctness gate: `maxima-load-all` has zero hard failures and all critical entrypoints are bound (`$integrate`, `$ratsimp`, `$factor`, `$solve`, `$limit`, `$determinant`).
- Genericity gate: fixes are CL-semantic and reusable across non-Maxima Lisp code (no Maxima-only patch paths).
- Performance gate: 2x speedup on Maxima workload suite versus current baseline (tracked in reproducible scripts).
- Stability gate: no JIT indirect-call crashes, no masked errors, no fallback-only behavior.
- Drift gate: Habu detects `../hoist` API changes with explicit contract tests.

## Tree

### A. Maxima Fast Track
- [ ] `habu-maxima-fast-exec-049ee786` Maxima fast execution plan.
  - [ ] `habu-define-maxima-gates-aca4e665` Define machine-checkable Maxima correctness and perf gates.
  - [ ] `habu-audit-loader-failures-fda25dca` Audit and expose loader per-form failures and binding gaps. Depends on `habu-define-maxima-gates-aca4e665`.
  - [ ] `habu-close-cl-semantic-dac2c058` Close CL semantic gaps blocking generic Maxima execution. Depends on `habu-audit-loader-failures-fda25dca`.
    - [x] `habu-fix-fn-designators-c2cf5df2` Fix function designator semantics (`coerce`/`fdefinition`) with regressions.
    - [ ] `habu-fix-pkg-semantics-949bd125` Fix package semantics mismatches (`defpackage`/import/shadow/use).
    - [ ] `habu-fix-macro-expansion-35b2e63f` Fix macro expansion edge semantics for large sources.
  - [ ] `habu-reader-parser-parity-a7ceffb7` Close reader/parser parity gaps (`#.` and `#nA` terminal parsing). Depends on `habu-define-maxima-gates-aca4e665`.
  - [ ] `habu-stabilize-eval-vm-d1c1c5cc` Stabilize eval/VM paths under macro-heavy Maxima workloads. Depends on `habu-close-cl-semantic-dac2c058`.
    - [ ] `habu-rca-indirect-call-d9f594ad` RCA and fix JIT indirect-call path root cause (no workaround).
    - [ ] `habu-fix-nested-eval-420ba9e0` Fix nested eval/non-local exit frame restoration. Depends on `habu-rca-indirect-call-d9f594ad`.
    - [x] `habu-design-safe-macro-e2cbd352` Design/implement safe macro-expander caching with stable chunk/index semantics (no transient chunk-pool assumptions).
  - [ ] `habu-maxima-load-to-e6d01b9c` Drive Maxima loader and critical symbol binds to green. Depends on `habu-stabilize-eval-vm-d1c1c5cc`.
  - [ ] `habu-profile-maxima-hotspots-977ac23d` Profile real Maxima hotspots in interpreter and JIT modes. Depends on `habu-maxima-load-to-e6d01b9c`.
  - [ ] `habu-raise-jit-coverage-4bfef8eb` Raise JIT coverage for Maxima hotspot call/data paths. Depends on `habu-profile-maxima-hotspots-977ac23d`.
    - [ ] `habu-jit-missing-call-7abc44ab` Add generic JIT lowering for missing call-target patterns.
    - [ ] `habu-jit-missing-data-714eb838` Add generic JIT lowering for missing vector/hash/string hot ops.
  - [ ] `habu-cut-vm-gc-511ec7d3` Cut VM/GC overhead in long CAS workloads. Depends on `habu-raise-jit-coverage-4bfef8eb`.
    - [x] `habu-reduce-gc-root-04a18d48` Reduce GC root assembly overhead in collection paths.
    - [ ] `habu-shrink-transient-allocs-d4dbcf28` Shrink transient allocations in hot eval/VM paths.
    - [x] `habu-gc-architecture-upgrade-4f113b2e` Upgrade GC architecture for lower pause/copy cost and lower RSS.
      - [x] `habu-gc-telemetry-gates-1e9aa49f` Add phase-level GC telemetry and Maxima perf gates.
      - [x] `habu-persist-gc-state-10a4377a` Persist GC state/work queues across collections.
      - [x] `habu-root-slot-idx-582a4cc2` Add persistent root-slot index and dirty-epoch rebuild control. Depends on `habu-persist-gc-state-10a4377a`.
      - [x] `habu-nursery-layout-scaffold-7aa479dc` Add nursery/tenured/LOS heap layout scaffolding. Depends on `habu-persist-gc-state-10a4377a`.
      - [x] `habu-write-barrier-stores-2b8bf449` Add write barriers to all pointer mutators. Depends on `habu-nursery-layout-scaffold-7aa479dc`.
      - [x] `habu-remembered-set-c9541b7e` Add card table + remembered set scanning APIs. Depends on `habu-write-barrier-stores-2b8bf449`.
      - [x] `habu-minor-gc-collector-2f89a428` Implement minor GC with promotion policy. Depends on `habu-remembered-set-c9541b7e`.
      - [x] `habu-tenured-collector-1dc6f7a9` Implement non-moving tenured mark-sweep collector. Depends on `habu-minor-gc-collector-2f89a428`.
      - [x] `habu-large-obj-space-a5bd4ea3` Implement large-object space and pinning semantics. Depends on `habu-minor-gc-collector-2f89a428`.
      - [x] `habu-vm-jit-barrier-0df52611` Wire VM/JIT store paths to barrier/safepoint hooks. Depends on `habu-write-barrier-stores-2b8bf449`.
      - [x] `habu-gc-regression-perf-91ce5f3c` Add GC regression and throughput gates. Depends on `habu-minor-gc-collector-2f89a428`.
  - [x] `habu-lock-hoist-api-0d6259d1` Lock `../hoist` API drift handling in Habu-side contract checks.
    - [x] `habu-hoist-api-contract-6bac1b3e` Add compile/runtime contract probes for hoist interface.
  - [ ] `habu-perf-ci-and-2b7ac2f9` Add perf regression gates and unified docs. Depends on `habu-cut-vm-gc-511ec7d3`, `habu-lock-hoist-api-0d6259d1`.

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
  - [x] `habu-fix-jit-gate-e7562d33` Restore JIT gate integrity (default hoist backend + source-backed jit bench + strict bench-check args).
  - [x] `habu-reverify-hoist-compile-b48554f1` Reverify hoist compile gate after latest upstream rebuild.

### 2. Compiler Core
- [ ] `habu-fix-loop-macro-c7a41441` Fix LOOP macro dispatch.
- [x] `habu-fix-loop-loop-daf318dd` Fix LOOP conditional `DO` multi-form parsing and `loop-finish` lowering in extended clauses. Depends on `habu-fix-loop-macro-c7a41441`.
- [x] `habu-support-loop-in-84a5efed` Support `loop for ... in ... by ...` step-function clauses.
- [x] `habu-loop-else-when-9b45625b` Support `loop ... when ... else when ... else ...` conditional routing.
- [x] `habu-iterative-cond-lowering-fa7ea387` Lower large COND forms iteratively to reduce compiler recursion overhead.
- [ ] `habu-support-loop-for-6e9d9623` Support LOOP `for ... and ...` clauses. Depends on `habu-fix-loop-macro-c7a41441`.
- [ ] `habu-support-setf-bit-b72546e8` Support `(setf (bit/sbit ...))` places.
- [x] `habu-support-setf-composed-7c79e463` Support composed list places in `setf` (`cadr`/`cddr`/`caddr`/`cdddr`/aliases).
- [ ] `habu-support-setf-generic-67036246` Support generic names `(setf foo)` in DEFGENERIC/DEFMETHOD.
- [ ] `habu-fix-setf-invalidsyntax-2e7560f2` Fix remaining `setf` InvalidSyntax in misc type-prop. Depends on `habu-signal-symbol-pkg-e766fbcf`.
- [ ] `habu-fix-concatenate-compiler-1e8d411f` Fix concatenate compiler fast-path semantics.
- [ ] `habu-fix-log-optional-de674bd9` Fix LOG optional base lowering. Depends on `habu-fix-concatenate-compiler-1e8d411f`.
- [ ] `habu-fix-defstruct-keyword-fe214c20` Fix DEFSTRUCT keyword/`:conc-name` parsing.
- [ ] `habu-fix-defstruct-invalid-85b8fcf9` Fix remaining DEFSTRUCT InvalidSyntax paths.
- [ ] `habu-fix-defstruct-copier-952e241d` Fix DEFSTRUCT copier fallback generation.
- [x] `habu-scope-special-declarations-33d29c18` Scope proclaimed `special` handling by symbol identity (package-aware) to avoid cross-package leakage.

### 3. Runtime / Package / Stream / Time
- [ ] `habu-fix-pathname-merge-71b041a8` Fix pathname merge for compile-file-pathname.
- [ ] `habu-guard-core-pkg-a8f23f9b` Guard deletion of core packages.
- [ ] `habu-fix-finish-output-15f73282` Fix finish-output/force-output stream designator behavior.
- [ ] `habu-add-encode-universal-fe1b93d9` Add encode-universal-time primitive + wiring.
- [ ] `habu-signal-symbol-pkg-e766fbcf` Signal SYMBOL-PACKAGE type errors as Lisp conditions.
- [x] `habu-fix-symbol-fn-f9fd590d` Fix function-namespace resolution so `symbol-function`/macro setup is not hijacked by special value bindings; seed function cells on `defun`/`setf` function definitions and revalidate Maxima readiness.
- [x] `habu-resolve-internal-setter-9122d08d` Classify `%aset`/`%svset`/`%sset` as builtin callable designators so bootstrap function resolution does not depend on nil-slot fallback behavior.

### 4. VM / GC / Eval / CLOS / Conditions
- [ ] `habu-fix-gc-chunk-7057f649` Fix GC chunk root corruption.
- [ ] `habu-fix-transitive-lambda-f02bd0d9` Fix transitive lambda capture lowering.
- [ ] `habu-fix-nested-eval-b0bbd02d` Fix nested eval non-local exits. Depends on `habu-fix-gc-chunk-7057f649`.
- [ ] `habu-fix-clos-superclass-2aa44685` Fix CLOS superclass alias resolution.
- [ ] `habu-fix-warn-apply-fe791fc7` Fix warn/apply nil callee path.
- [ ] `habu-fix-ansi-deftest-faa1296f` Fix ANSI DEFTEST TypeMismatch root cause.
  - [ ] `habu-trace-first-ansi-3501b989` Trace first uncaught ANSI TypeMismatch.
  - [ ] `habu-patch-ansi-typemismatch-dae30cf8` Patch root cause.
  - [ ] `habu-add-ansi-typemismatch-817bda8d` Add focused regression.
  - [ ] `habu-verify-ansi-progression-56a3eae2` Verify ANSI progression and update baseline artifacts.

### 5. Maxima Continuation
- [x] `habu-increase-default-heap-44a06bce` Increase default heap and build comprehensive Maxima loader.
- [ ] `habu-fix-maxima-cas-a491af14` Fix Maxima CAS operations: integrate, solve, factor, limit, ratsimp, det.
  - [x] `habu-maxima-subset-load-e9db9bb5` Maxima subset: load `db`/`compar` deps so `kindp` exists on CAS paths.
  - [x] `habu-rca-and-fix-4a4ea5d5` RCA and fix `$ratsimp` `setf: unsupported place` root cause.
  - [x] `habu-add-maxima-cas-1807f8ae` Add end-to-end CAS regression checks in integration tests.
  - [x] `habu-maxima-loader-fix-d654483f` Maxima loader: fix `server`/`coerce` crash so full module load can continue.
  - [x] `habu-fix-bigfloat-impl-dbf1cefb` Bind BIGFLOAT-IMPL shadow imports to callable operators (with inverse-trig fallbacks) so trig modules (`trigi`/`trigo`) load without unbound function designators.
  - [x] `habu-investigate-mapcar-cb-ad5def1b` RCA callback crash in Maxima `$errormsg`: fix stdlib `mapc` to CL variadic semantics and add regression.
  - [ ] `habu-maxima-integrate-path-b786024b` Maxima integrate path: resolve post-loader integrate failure chain. Depends on `habu-investigate-mapcar-cb-ad5def1b`, `habu-fix-fn-designators-c2cf5df2`.
    - [x] `habu-trace-integrate-unbound-53804676` Trace integrate unbound-variable root and lock dependency-chain regression (`alias`/`sinint` + live `$integrate` call).
    - [x] `habu-auto-detect-maxima-d2876566` Auto-detect Maxima source root and fail fast when source fixtures are missing.
    - [x] `habu-fix-cond-signal-4f85b2c8` Fix `(signal ...)` lowering so unhandled conditions return nil instead of THROW control-error.
    - [x] `habu-fix-declare-top-e3668a14` Honor proclaimed `special` lambda params via dynamic bindings (`progv`) so `declare-top` state is visible in helper callees (`define-mode`/`defs1`) and `db.lisp` `defmode`/`clear` load path no longer fails at function-definition time.
    - [x] `habu-propagate-load-form-d898e591` Propagate load parse/eval errors instead of silently continuing forms; add strict-load regression.
    - [x] `habu-fix-nested-load-d7d28e45` Fix nested `load` non-local exit relay so `handler-case` around `load` aborts on first error instead of resuming later file forms.
    - [x] `habu-revalidate-integrate-with-0874ce3e` Revalidate integrate path end-to-end once real Maxima source fixtures are present again. Depends on `habu-fix-declare-top-e3668a14`.
  - [ ] `habu-maxima-factor-ratsimp-521dd2ca` Maxima factor/ratsimp path: fix TypeMismatch and ProgramError roots.
  - [x] `habu-maxima-core-loader-999c7eb3` Add Maxima core subset loader + entrypoint binding integration gate.
  - [x] `habu-rca-load-stackoverflow-e3d4f5d8` RCA and fix load stack overflow path for Maxima large source files.
  - [x] `habu-fix-sin-lisp-b34b817f` Fix `sin.lisp` load root so `SININT` is bound and integrate path can complete. Ensure `schatc` dependency chain is loaded (`m2`/`schatchen-cond` present) before integrate execution.
  - [x] `habu-add-internal-option-8cbd6feb` Add system-only/internal keyword controls for loader diagnostics and bind checks. Dependency for `habu-maxima-end-to-efe58661`.
- [ ] `habu-maxima-end-to-efe58661` Maxima end-to-end integration test continuation. Depends on `habu-fix-maxima-cas-a491af14`, `habu-maxima-subset-load-e9db9bb5`, `habu-rca-and-fix-4a4ea5d5`, `habu-add-maxima-cas-1807f8ae`, `habu-cut-gc-root-25d3bb03`, and `habu-fix-hoist-compile-9a100641`.

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
