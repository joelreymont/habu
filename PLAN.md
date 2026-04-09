# PLAN — Generic Common Lisp Closure For Maxima

Last updated: 2026-04-09
Owner: Habu runtime/compiler/reader/VM/JIT
Review baseline: round 1 findings incorporated

## 1. Goal

Habu must load and run upstream Maxima because Habu implements the required Common Lisp semantics generically.

This plan is done only when:
- upstream Maxima loads from a declared trusted root with no semantic patch layer,
- interactive/scripted/batch/test-batch execution works on generic Habu,
- remaining failures are expressed as concrete Habu language/runtime gaps,
- performance is measured only on the clean path,
- benchmark truthfulness is fail-closed.

## 2. User Goal Mapping

- G1: load upstream Maxima with no Maxima-only semantic cheats
- G2: run real Maxima workloads and canonical tests
- G3: make correctness measurable through canonical Maxima paths
- G4: optimize only after correctness is real and observable

## 3. Non-Negotiable Constraints

- Hard cutover only. No legacy support, no fallback paths, no silent substitutions.
- No Maxima-only semantic overlays to close blockers.
- No source substitution for `.fasl` / `.hfasl`.
- No ambient executable roots such as `/tmp/maxima/**` or `~/.maxima/**`.
- No benchmark result is valid unless loader/run correctness gates are green.

## 4. End-State Gates

- E1: `lib/maxima-early-patches.lisp` is removed.
- E2: `lib/maxima-post-load.lisp` is removed or reduced to non-semantic bootstrap only.
- E3: `lib/maxima-stubs.lisp` is removed or reduced to declarations that do not change semantics.
- E4: package/function/macro/special lookup is canonical by symbol identity and package state.
- E5: loader/package/reader/stream/pathname/condition semantics are sufficient for upstream Maxima core and targeted share modules.
- E6: upstream `run_testsuite` / `test-batch` pipeline is the correctness authority.
- E7: benchmark harness rejects dirty loader states and reports truthful JIT eligibility/coverage.

## 4.1 Current State

Closed enough to treat as done unless regressions reopen them:
- Maxima patch overlays/stubs/fallback lookup ladders were cut back to canonical paths.
- fake `.fasl` source substitution is gone.
- eval-based builtin wrapper synthesis is gone.
- restart lookup/invoke and generic handler dispatch landed.
- package bootstrap, loader specials, stream/pathname unification, synonym streams, long loads, structure/object split, BOA constructors, and `defstruct` printer hooks landed.
- `get-macro-character` lowering/runtime contract is fixed.
- `#p` now reads as a real pathname object and pathname literals are self-evaluating, which moved `intl.lisp` and canonical `rtest6` past the old localization/pathname lie.

Live blockers proven right now from the current runner path, which is still provisional until `1.1` and `2.4` land:
- `tools/maxima-rtest.lisp rtest6` now reaches real suite execution, but the front-door floor starts earlier at the `rtest6.mac:94-108` string/array constructor path before the later `infixie` parse failure at `rtest6.mac:110`.
- `rtest6` still has an older compiler-stage dot for real macro/destructuring failure once the parser/operator floor is lifted: `habu-fix-rtest6-macro-de33e2c1`.
- NLX correctness remains open and must be treated as a likely shared cause for later `test-batch`/error paths: `habu-fix-nlx-control-dcb701b2`.
- performance work remains explicitly blocked on clean canonical execution.
- Current `rtest6` floor is no longer the old generic `unwind-protect` cleanup bug. That part is fixed in `src/interp/vm.zig` by switching condition-transfer detection to full `Vm.State` snapshots, with proof from the generic unwind regression and the Maxima `meval*` cleanup regression.
- The live `rtest6` blocker is now a global-state mismatch on Maxima's `+labs` bookkeeping: after `errcatch` around `integrate`, `symbol-plist` on `*Z*` contains `(+LABS $ZERO)` while `symbol-value` of `MAXIMA::+LABS` is still `nil`. `compar.lisp:dmark` writes both surfaces together, so the remaining root cause belongs in Habu's qualified global-slot identity/synchronization path (`src/interp/vm.zig`, `src/interp/repl.zig`, `src/compiler/compile.zig`), not in another Maxima semantic patch.
- Short-circuit compiler walkers must root their live list tails across recursive sub-compilation. `src/compiler/compile.zig:7030-7145` currently keeps `and`/`or` rest lists in raw locals while compiling the head clause, so moving GC can invalidate the recursive tail and crash in later cons dereferences before the real `+labs` floor is even observable.
- Built-in type dispatch must canonicalize same-pname package symbols back to the canonical `COMMON-LISP` type symbol before `typep`/`subtypep` matching. `MAXIMA` legitimately shadows `FLOAT` for its function namespace, but real upstream code still uses `'float` as a type designator in `../maxima/src/hypergeometric.lisp`, so Habu must resolve the built-in type symbol generically instead of treating `MAXIMA::FLOAT` as unknown.
- The old `BIGFLOAT` loader gate is closed. After fixing generic dispatcher helper-name collision in `src/compiler/compile.zig`, the narrowed repro now proves `(method-function ...)` for the `(NUMBER NUMBER)` method and generic `(bigfloat::two-arg-/ 1 2)` both return `1/2`, and canonical `tools/maxima-rtest.lisp rtest6` advances past the dirty-loader refusal into the real `test-batch` floor at problem 2 line 11.
- The current `test-batch` front-door break is no longer an `errset` mystery. The exact runner shape shows `errset` returning `nil` because `test-batch` really does signal an `ERROR`, and the reduced repro is `copy-seq` on a fill-pointer string buffer from `../maxima/src/nparse.lisp:324-333`. Habu's `length` primitive in `src/interp/vm.zig` is using vector capacity instead of fill pointer for sequence length, so `copy-seq` iterates past live characters, `char` faults on uninitialized tail slots, and canonical `rtest6` falls over while scanning later string forms.
- The remaining `rtest6` scientific-string cluster at lines `163/166/169/172`, plus `parse_string(string(most_positive_float))`, belongs in the generic float printer, not in Maxima logic. `src/runtime/primitives/io.zig:79-94` currently prints finite floats with plain `{d}` unless formatting overflows, so values that should round-trip through exponent notation come out as long fixed-point decimals like `"20000000.0"` and `"0.0009765625"`. The next formatter pass must switch to `%g`-style scientific thresholds generically and prove readback through the canonical Maxima forms.
- Lambda-local `declare (special ...)` still needs full parity with proclaimed specials. `src/compiler/compile.zig:4575-5292,6298-6306,15472-15584` currently seeds lambda parameter special bindings only from global proclamations seen during lambda-list parsing, but body declarations are filtered later. That leaves locally declared special lambda params compiled as globals/lexicals without the matching entry `progv` binding, which reproduces generically with `(funcall (eval '(lambda (x y z) (declare (special x y z)) x)) '(a) 2 3)` and surfaces in Maxima `tellsimp` rule closures at `rtest6.mac:76`.
- Common Lisp complex numbers still use a float-only backing store in `src/runtime/objects.zig`, `src/runtime/heap.zig`, `src/interp/vm.zig`, and `src/runtime/primitives/arith.zig`. That loses exact fixnum/rational parts at construction time, so Maxima `nformat` sees `(realpart #C(1 2)) => 1.0` and prints `2.0*%i+1.0` instead of `2*%i+1`. The cutover has to make complex parts first-class `Value`s, scan them in GC, print them recursively, and keep arithmetic/equality/hash logic generic across exact and inexact parts.

## 4.2 Remaining Execution Order

Do the remaining work in this order:
1. Remove parser/read-state blockers that stop canonical `rtest6` before semantic comparison.
2. Remove compiler/runtime blockers exposed next by canonical `rtest6`/`rtest6b`.
3. Finish NLX/condition/test-batch correctness so canonical batch execution is trustworthy.
4. Sweep core tranche A and reduce every remaining failure to concrete Habu gaps with follow-up dots.
5. Sweep share tranche A the same way.
6. Re-run interactive/scripted Maxima proof after each tranche to keep the load/run surface honest.
7. Only after canonical correctness is stable, do hoist/JIT/GC/perf work against real Maxima and Todd-Coxeter workloads.

## 5. Critical Path

### Phase 0 — Remove False Progress Layers

#### 0.1 Remove Maxima patch overlays and loader-side semantic fixups
- Goal: G1
- Files: `lib/maxima-early-patches.lisp:1-17`, `lib/maxima-loader.lisp:28-29`, `lib/maxima-loader.lisp:161-171`, `lib/maxima-post-load.lisp:14-109`
- Depends on: none
- Work:
  - delete early patch loading,
  - delete loader-time semantic rewrites,
  - delete post-load overrides that change reader/parser/autoload behavior,
  - delete post-load user-home executable search-root injection such as prepending `~/.maxima/**` into authoritative Maxima search paths,
  - reclassify anything retained as bootstrap-only and prove it is non-semantic.
- Acceptance:
  - no load path calls `lib/maxima-early-patches.lisp`,
  - `maxima-loader` and post-load code contain no semantic override of upstream Maxima functions/macros,
  - authoritative Maxima load/test/bench paths do not prepend or execute from `~/.maxima/**`,
  - remaining bootstrap code is limited to environment declaration and path manifest wiring.
- Risk:
  - many currently “working” loads will fail immediately; that is desired signal.
- Effort: L

#### 0.2 Remove `lib/maxima-stubs.lisp` semantic stand-ins
- Goal: G1
- Files: `lib/maxima-stubs.lisp:2-34`, `lib/maxima-stubs.lisp:97-215`, `lib/maxima-stubs.lisp:265-340`
- Depends on: 0.1
- Work:
  - delete fake packages/APIs,
  - delete bigfloat/numeric/operator fallback bindings,
  - delete simplified macro/function substitutes,
  - if any declarations remain, they must not alter behavior and must be explicitly justified.
- Acceptance:
  - no semantic fallback operators/macros/functions remain in `lib/maxima-stubs.lisp`,
  - load failures surface as Habu gaps, not stubbed behavior.
- Risk:
  - exposes real missing CL/runtime features quickly.
- Effort: L

#### 0.3 Remove legacy lookup fallback semantics
- Goal: G1
- Files: `src/runtime/heap.zig:416-430`, `src/runtime/heap.zig:3274-3351`, `src/runtime/heap.zig:4684-4698`, `src/compiler/compile.zig:1687-1752`, `src/compiler/compile.zig:7168-7189`, `src/compiler/compile.zig:9436-9498`, `src/compiler/compile.zig:10843-10884`, `src/compiler/compile.zig:13313-14060`, `src/interp/repl.zig:1487-1541`, `src/interp/repl.zig:1742-1767`, `src/interp/vm.zig:2465-2498`
- Depends on: none
- Work:
  - delete legacy symbol-table fallback,
  - delete unqualified/`CL`/`COMMON-LISP`/`CL-USER` retry logic,
  - delete case/name/% alias fallback builtin resolution,
  - delete unqualified-first special/global lookup,
  - delete helper/JIT/global-ref lookup fallbacks such as raw `heap.symbols.get` and manual uppercase retry outside canonical package lookup,
  - delete compiler lexical/function lookup fallback from exact symbol keys to bare-name/case-insensitive matching,
  - delete compiler-emitted CLOS fallback ladders for `MAKE-INSTANCE`, `CALL-NEXT-METHOD`, `INITIALIZE-INSTANCE`, `NO-NEXT-METHOD`, and `NO-APPLICABLE-METHOD`.
- Acceptance:
  - package-qualified and current-package semantics are the only lookup semantics,
  - builtin resolution is canonical by symbol identity only,
  - package-qualified references cannot resolve to unrelated local bindings by bare-name fallback,
  - interpreter/helper/JIT/global-ref lookup surfaces use the same canonical package-aware identity rules.
- Risk:
  - will break tests that codify old fallback behavior.
- Effort: L

#### 0.4 Replace side-effecting lookup APIs with read-only canonical queries
- Goal: G1
- Files: `src/interp/repl.zig:1512-1541`, `src/runtime/heap.zig:3351-3372`, `src/runtime/primitives/package.zig:567-611`, `src/runtime/primitives/package.zig:665-692`
- Depends on: 0.3
- Work:
  - separate lookup from interning,
  - ensure package symbol queries do not mutate CL/CL-USER/native tables,
  - audit call sites that currently “look up” by interning.
- Acceptance:
  - symbol/function/macro lookup performs no package mutation,
  - package growth occurs only through explicit intern/import/export operations.
- Risk:
  - hidden mutation dependencies may surface in reader/compiler/bootstrap.
- Effort: M

#### 0.5 Rewrite legacy tests to assert cutover behavior
- Goal: G1, G3
- Files: `src/runtime/heap.zig:4684-4698`, related package/lookup tests under `src/runtime/primitives/package.zig`, `src/interp/repl.zig`, `src/interp/vm.zig`
- Depends on: 0.3, 0.4
- Work:
  - delete tests that bless fallback behavior,
  - replace with package-only identity and no-mutation invariants.
- Acceptance:
  - tests fail if fallback semantics reappear.
- Risk:
  - existing test names may hide legacy assumptions.
- Effort: M

#### 0.6 Fix macro-table GC keying at the root cause
- Goal: G1, G2
- Files: `src/interp/repl.zig:4209-4255`, macro-table storage/GC refresh sites in `src/interp/repl.zig`, `src/interp/vm.zig`, `src/runtime/gc.zig`
- Depends on: none
- Work:
  - stabilize macro-table keys across moving GC or rekey them correctly,
  - remove O(n) `lookupMacroByName` fallback.
- Acceptance:
  - macro lookup uses stable canonical keys only,
  - no name-scan fallback remains.
- Risk:
  - adjacent literal/chunk/root tables likely share the same failure mode.
- Effort: L

#### 0.3a Fix CLOS protocol emission, reflective lifecycle/introspection, package identity, and helper naming
- Goal: G1, G2
- Files: `src/compiler/compile.zig:2093-2130`, `src/compiler/compile.zig:12706-12759`, `src/compiler/compile.zig:13224-13295`, `src/compiler/compile.zig:13310-13324`, `src/compiler/compile.zig:13682-13762`, `src/compiler/compile.zig:13955-14065`, `src/compiler/compile.zig:14453-14469`, `lib/stdlib.habu:6989-7065`, `lib/stdlib.habu:7559-7595`
- Depends on: 0.3
- Work:
  - make compiler-emitted CLOS protocol calls thread the real generic-function, method, and initarg information through `initialize-instance`, `call-next-method`, `no-next-method`, and `no-applicable-method`,
  - replace reflective CLOS lifecycle stubs such as `allocate-instance`, `reinitialize-instance`, `change-class`, and `ensure-generic-function` with real protocol semantics or explicit non-advertised absence,
  - replace reflective generic-function/method introspection and mutation stubs such as `find-method`, `remove-method`, `compute-applicable-methods`, `function-keywords`, and `make-instances-obsolete` with real semantics or explicit non-advertised absence,
  - keep compiler-owned generic-function registries in sync with package identity across rename/delete/imported-class cases,
  - make method specializer identity and synthesized method helper globals package-unique and collision-free,
  - make `make-instance` accept real CL class designators instead of stripping arbitrary conses down to a symbol or rejecting computed class designators,
  - resolve synthesized constructor/helper globals from the class symbol's home package or canonical class metadata instead of the current package,
  - remove synthetic or placeholder protocol calls that use `nil`, interned names, or dropped initargs in place of the real protocol objects.
- Acceptance:
  - compiler-emitted CLOS protocol calls carry the same semantic payload as the CL protocol requires,
  - reflective CLOS entrypoints no longer return placeholder results that ignore allocation, initargs, slot migration, generic-function creation, or reflective method/GF state,
  - compiler-owned CLOS registries do not fork from runtime package state,
  - same-local-name classes or methods in different packages do not collide in specializer ordering or helper globals,
  - `(make-instance (find-class 'pkg::foo) ...)` and imported/inherited class designators compile to the same class/constructor that canonical runtime lookup would use,
  - method dispatch and `call-next-method` failure paths do not rely on placeholder globals or name-only stand-ins.
- Risk:
  - CLOS fallback cleanup can look complete while protocol semantics are still wrong.
- Effort: L

#### 0.7 Remove fake FASL success paths
- Goal: G1, G3
- Files: `src/interp/repl.zig:1945-1991`, `src/interp/repl.zig:2317-2338`, `lib/stdlib.habu:7475-7492`
- Depends on: none
- Work:
  - delete sibling-source substitution,
  - delete fake-success `compile-file` / `compile-file-pathname` behavior that returns pathname metadata without a real emitted artifact,
  - implement or fail-close object reconstruction semantics needed by real compiled outputs, including `make-load-form` and `make-load-form-saving-slots`,
  - make `.fasl` / `.hfasl` execute real semantics or fail explicitly.
- Acceptance:
  - a `.fasl` load never silently loads source,
  - `compile-file` and `compile-file-pathname` never report success without a real artifact that the loader can consume canonically,
  - compiled object reconstruction does not rely on hard stubs for `make-load-form` / `make-load-form-saving-slots`,
  - benchmark and loader scripts observe explicit success/failure.
- Risk:
  - some Maxima/share paths may currently rely on the lie.
- Effort: M

#### 0.8 Remove non-canonical builtin and function-designator wrapper synthesis
- Goal: G1, G4
- Files: `src/interp/repl.zig:1435-1632`, `src/compiler/compile.zig:8241-8364`, `src/compiler/compile.zig:18931-18933`
- Depends on: 0.3, 0.4
- Work:
  - represent builtins as first-class callable values or direct VM dispatch,
  - delete wrapper lambdas that call `eval`,
  - close the remaining direct-callable gap for nullary/variadic/optional builtins so `symbol-function`/`fdefinition` work before stdlib bootstrap for forms such as `intern`, `append`, `member`, `assoc`, `find`, `position`, `count`, `remove`, and `substring`,
  - close the remaining direct-callable gap for exact package-qualified compiler-recognized operators such as `COMMON-LISP:MAKE-INSTANCE` so reflective CLOS paths (`symbol-function`, `apply`, `make-condition`) use the same canonical callable surface as direct calls,
  - delete compiler-side builtin lambda synthesis and function-position symbol-designator fallback that postpones canonical callable resolution to runtime.
- Acceptance:
  - builtin/function designators resolve canonically without runtime-generated wrappers,
  - compiler lowering does not synthesize wrapper semantics that diverge from the real fdefinition surface.
- Risk:
  - impacts `funcall`, `apply`, symbol-function, and compiler callable lowering.
- Effort: L

#### 0.10 Remove JIT bridge OOM fallback semantics
- Goal: G1, G3, G4
- Files: `src/interp/vm.zig:2173-2240`, `src/tests/integration.zig:10826-10910`, `tools/validate-session:188-199`
- Depends on: 0.5, 0.8
- Work:
  - make JIT bridge OOM/error results explicit instead of returning interpreted fallback,
  - delete fallback counters and fallback-oriented test expectations,
  - make validation profiles assert clean JIT eligibility/behavior without blessing fallback execution.
- Acceptance:
  - JIT bridge runtime never degrades to interpreted execution on OOM,
  - tests and validation fail if fallback semantics reappear,
  - Maxima validation profiles no longer depend on fallback-behavior checks.
- Risk:
  - exposes real JIT heap/bridge/root pressure that fallback used to hide.
- Effort: M

#### 0.9 Remove condition error masking
- Goal: G1, G2
- Files: `src/interp/vm.zig:9397-9444`
- Depends on: none
- Work:
  - propagate allocation/runtime failures from condition signaling instead of degrading silently.
- Acceptance:
  - no `catch return false` style masking remains on condition signal path.
- Risk:
  - may expose wider unwind/restart correctness gaps.
- Effort: S

### Phase 1 — Establish A Clean Maxima Load Surface

#### 1.1 Define a single authoritative Maxima manifest and trusted-root contract
- Goal: G1, G2, G3
- Files: `lib/maxima-manifest.lisp:1-110`, `lib/maxima-loader.lisp:6-26`, `lib/maxima-loader.lisp:30-159`, `lib/maxima-post-load.lisp:1-90`, `tools/maxima-rtest.lisp:1-83`, `tools/validate-session:1-260`, `bench/maxima_workload.zig:373-401`, `bench/maxima_workload.lisp:1-120`, `src/main.zig:47-102`, `src/interp/repl.zig:169-170`, `src/interp/repl.zig:1416-1438`, `src/interp/repl.zig:1718-1718`, `src/interp/repl.zig:2216-2237`, `lib/stdlib.habu:7319-7347`
- Depends on: 0.1, 0.2
- Work:
  - define one source of truth for Maxima root, module manifest, search roots, and autoload scope,
  - make loader/scripts/tests/bench share the same manifest,
  - make `lib/maxima-manifest.lisp` the only authoritative source of Maxima root/module/search-root truth,
  - ban writable ambient roots such as `/tmp/maxima/**` and user-home executable search roots such as `~/.maxima/**`,
  - ban cwd/default-path executable discovery for logical pathname translation files unless they live under the trusted manifest roots,
  - ban raw relative `load` / autoload / batch execution outside the trusted loader context,
  - ban repo-helper/bootstrap resolution through ambient cwd/default search; authoritative entrypoints must derive repo root from their own truename/startup context and pin `lib/stdlib.habu`, `lib/maxima-loader.lisp`, `lib/maxima-post-load.lisp`, and benchmark helpers to canonical repo-root paths,
  - require one canonical authoritative bootstrap entrypoint/sequence that owns manifest capture, trusted-root setup, package-init/stub/post-load/testsuite wiring, and forbid authoritative tools from hand-stitching extra bootstrap steps around it,
  - remove or demote runtime trust-boundary mutation APIs such as `%add-trusted-load-root`; authoritative `trusted_load_roots` must be fixed and validated before any Lisp helper/module executes,
  - seal the manifest/root-candidate snapshot before any Lisp helper/module executes and forbid in-process refresh or mutation of authoritative manifest/root-candidate globals on authoritative paths,
  - make manifest-selected bootstrap helpers mandatory on authoritative paths; no `probe-file`/`boundp`-gated elision, opportunistic reload, or best-effort skip may preserve a nominally authoritative bootstrap,
  - clear, sandbox, or explicitly pin ambient user-home runtime state such as `*maxima-userdir*` / `$maxima_userdir` for authoritative load/test/bench flows,
  - remove runtime seeding of trusted executable roots from launch CWD in the load engine itself,
  - normalize and contain all generic relative-load resolution within trusted roots,
  - delete basename-trim and secondary-candidate guessing from generic `load` resolution,
  - require every authoritative upstream Maxima file load/open to resolve the target to a real regular file and re-check containment under the sealed trusted Maxima root before execution,
  - require authoritative upstream file loads/opens to enforce containment and no-symlink policy in a race-free open/load operation, not a separate prevalidation step,
  - require bench/test output to record which trusted Maxima root was used,
  - bind authoritative bootstrap to exact content identity of semantic helper files outside the upstream Maxima module list, including loader/post-load/stub/package-init helpers.
- Acceptance:
  - one manifest drives loader, bench, and test tooling,
  - trusted roots are explicit and validated,
  - no host-specific or ambient root guessing remains,
  - no `~/.maxima` or user-home search-root injection remains on authoritative paths,
  - authoritative runs cannot widen `trusted_load_roots` from loaded code at any point in bootstrap or steady state,
  - authoritative manifest/root identity comes from a sealed bootstrap snapshot, not a mutable live Lisp global,
  - authoritative bootstrap hard-fails if any required manifest/package-init/bootstrap helper is missing, unreadable, skipped, or reloaded opportunistically,
  - authoritative script/test/bench entrypoints do not run bespoke pre-bootstrap sequences outside the canonical bootstrap owner,
  - authoritative runs do not inherit ambient writable home-root state through `*maxima-userdir*` / `$maxima_userdir`,
  - runtime trusted-root state is never silently seeded from launch CWD,
  - logical pathname translation loading cannot execute files from cwd or undeclared defaults,
  - ordinary relative loads either resolve through the trusted loader context or fail explicitly,
  - generic relative load resolution cannot escape trusted roots by `..`, non-canonical truenames, or guessed alternate candidates,
  - authoritative upstream Maxima file loads cannot follow symlink/non-regular/out-of-root escapes inside the trusted tree,
  - authoritative upstream file containment checks cannot be bypassed by check/use races on ancestor or leaf paths,
  - authoritative entrypoints fail if repo helpers resolve outside the repo root or via ambient cwd/default search,
  - `bench/maxima_workload.lisp` and validation helpers do not hardcode ambient `../maxima/**` or `/tmp/**` executable proof paths,
  - benchmark/test output includes trusted-root provenance, exact bootstrap-helper content provenance, and a content fingerprint for the trusted upstream Maxima tree actually loaded.
- Risk:
  - current workflows may assume convenient ad-hoc roots.
- Effort: L

#### 1.2 Fix package bootstrap ordering and remove reader-side package auto-creation
- Goal: G1, G2
- Files: `src/reader/parser.zig:1016-1040`, `src/reader/parser.zig:1168-1184`, `src/runtime/primitives/package.zig:315-316`, `../maxima/src/maxima-package.lisp`
- Depends on: 1.1
- Work:
  - implement correct reader semantics for `pkg:sym` versus `pkg::sym`,
  - require packages to exist before qualified reads,
  - make Maxima package bootstrap happen explicitly and early,
  - stop the reader from fabricating native packages on sight.
- Acceptance:
  - single-colon package reads enforce external accessibility,
  - double-colon package reads perform internal access only,
  - qualified symbol read fails clearly if package bootstrap is missing,
  - `maxima-package.lisp` bootstraps package state needed by subsequent loads.
- Risk:
  - early-source ordering bugs will surface immediately.
- Effort: M

#### 1.3 Collapse package state to one canonical representation
- Goal: G1, G2
- Files: `src/runtime/primitives/package.zig:316-330`, `src/runtime/primitives/package.zig:642-656`, `src/runtime/primitives/package.zig:1711-1716`, `src/runtime/heap.zig:3326-3339`, `src/runtime/heap.zig:3522-3595`
- Depends on: 0.3, 0.4, 1.2
- Work:
  - remove dual mutable sources of truth for package/export/import state,
  - remove native-placeholder/Lisp-package drift,
  - root current-package state correctly across GC/load,
  - remove silent repair/reset of package context to `CL-USER`/`CL`/`null`.
- Acceptance:
  - package lookup/export/import cannot diverge across backing structures,
  - package lookup returns one canonical symbol identity from one canonical package state,
  - package context is preserved or fails explicitly; it is never silently reset to a fallback package.
- Risk:
  - touches import/export/inherit behavior broadly.
- Effort: L

#### 1.3b Make destructive package operations preserve canonical invariants
- Goal: G1, G2
- Files: `src/runtime/primitives/package.zig:953-1075`, adjacent package-registry/update sites touched by rename/delete/nickname/use-list mutation
- Depends on: 1.3
- Work:
  - make `rename-package`, `delete-package`, nickname replacement/removal, use-list cleanup, current-package invalidation, and symbol home-package updates preserve one canonical package state,
  - migrate or purge qname-keyed globals and package-qualified `class_metadata` when package names change or packages are deleted,
  - forbid registry drift between package maps, alias maps, use-lists, and symbol metadata after destructive package mutations.
- Acceptance:
  - destructive package operations do not leave divergent registry state behind,
  - current package either stays canonical or fails explicitly after package destruction/rename,
  - rename/delete paths do not orphan globals or `class_metadata` under old package-name keys,
  - focused regressions prove no package/alias/use-list drift survives rename/delete paths.
- Risk:
  - destructive package ops can break later reader/load behavior far from the mutation site.
- Effort: M

#### 1.3a Make loader special state canonical and dynamic
- Goal: G1, G2, G3
- Files: `src/interp/repl.zig:2038-2066`, `src/interp/repl.zig:2116-2149`, `src/interp/repl.zig:2216-2237`, `lib/stdlib.habu:7319-7347`
- Depends on: 1.1, 1.2, 1.3
- Work:
  - make `load` dynamically and canonically bind `*LOAD-PATHNAME*`, `*LOAD-TRUENAME*`, `*DEFAULT-PATHNAME-DEFAULTS*`, and `*PACKAGE*`,
  - remove alias writes and cwd heuristics used in place of true dynamic special binding,
  - make VM `*PACKAGE*` and native reader package state reconcile atomically and fail closed when the VM package object cannot be mapped back to the canonical native package,
  - make VM reader entrypoints such as `read`, `read-from-string`, and stream-backed reader loops resync native reader package state from dynamic `*PACKAGE*` before interning any symbols,
  - save and restore package state by canonical package designator semantics rather than stale package-object reuse,
  - make nested loads and logical-pathname translation loading depend only on this contract,
  - make logical pathname translation loading fail explicitly on real translation-load errors instead of masking them or falling through to unrelated candidates,
  - ensure translation state is derived from the current trusted loader context, not stale preexisting global state.
- Acceptance:
  - nested `load`/autoload/batch flows get truthful dynamic special bindings,
  - path/package context is derived from loader state, not post-hoc alias repair,
  - package restoration updates both VM and reader package state or aborts the load,
  - reader entrypoints never intern into stale `CL-USER`/previous-package state while dynamic `*PACKAGE*` names some other canonical package,
  - stale, deleted, or recreated package objects cannot be silently restored into VM or reader state,
  - translation loading has no error-masking cross-candidate fallback after a real load failure.
- Risk:
  - this is load-bearing for nested loads, autoload, and canonical test execution.
- Effort: L

#### 1.4 Make function and macro lookup canonical, package-correct, and generic
- Goal: G1, G2
- Files: `src/interp/repl.zig:1565-1595`, `src/interp/repl.zig:4209-4255`, `src/compiler/compile.zig:16755-16801`, `../maxima/src/suprv1.lisp:144-175`, `../maxima/src/mlisp.lisp:2037-2117`
- Depends on: 0.3, 0.4, 0.6, 1.2, 1.3
- Work:
  - remove Maxima-specific autoload semantics from generic REPL lookup,
  - implement generic autoload/property lookup without `$`-prefix fallback,
  - make symbol-function/macro-function/special-variable lookup package-correct,
  - canonicalize plist/property-indicator identity for function/macro/autoload metadata such as `%FUNCTION-CELL`, `MACRO-FUNCTION`, `%HABU-MACRO-ENTRY`, `AUTOLOAD`, and `LOAD-FUNCTION`,
  - forbid ambient `heap.intern(...)` on property lookup/store paths and use one shared exact-symbol helper for plist-key resolution,
  - remove compiler-side macro canonicalization by package/name accessibility and make compile-time macro lookup use exact symbol identity.
- Acceptance:
  - generic lookup contains no `MAXIMA:AUTOLOAD`, `MAXIMA:LOAD-FUNCTION`, or `$`-prefix special case,
  - upstream Maxima autoload works via generic property/function semantics,
  - cross-package `fdefinition`, `macro-function`, and autoload property lookup use exact indicator identity before/after GC/load.
- Risk:
  - this is on the critical path for `mforma`, loader, and interactive evaluation.
- Effort: L

#### 1.4a Make `LOOP` expansion terminate on canonical `WHILE`/control IR
- Goal: G1, G2
- Files: `lib/stdlib.habu:4787-6202`, `src/interp/repl.zig:4479-4596`, `src/compiler/compile.zig:8520-8574`, `../maxima/src/globals.lisp:362-368`
- Depends on: 1.4
- Work:
  - prove whether `WHILE` is being treated as a macro, aliased macro key, or recursively reintroduced control form during `LOOP` expansion,
  - remove the `LOOP`/`WHILE` expansion cycle at the real boundary instead of capping recursion or special-casing Maxima forms,
  - make `LOOP ... WHILE ... DO ...` lower once onto canonical `WHILE`/block/tagbody control and stop there,
  - add focused regression coverage for the exact `effective-flonum-epsilon` shape and a minimal `LOOP WHILE DO` expansion/load path.
- Acceptance:
  - `(loop while test do form)` expands/compiles without recursive `LOOP`/`WHILE` macro re-entry,
  - `../maxima/src/globals.lisp` advances past `effective-flonum-epsilon` on the clean loader path,
  - no recursion-depth guard, argument cap workaround, or Maxima-local patch is involved.
- Risk:
  - the same expansion bug can hide behind other control macros and must be fixed at the generic expansion boundary.
- Effort: M

#### 1.4b Make `LOOP` clause ordering match ANSI per-iteration semantics
- Goal: G1, G2
- Files: `lib/stdlib.habu:4877-6202`, `src/tests/integration.zig`, `../maxima/src/intl.lisp:104-106`
- Depends on: 1.4a
- Work:
  - keep iterator exhaustion tests separate from `WHILE`/`UNTIL` guard tests,
  - run clause-generated per-iteration bindings such as `AS var = expr` before later guards consume them,
  - prove the exact upstream `intl.lisp` shape `loop for i upfrom ... below ... as c = ... while ...` terminates and returns,
  - keep the fix generic to all `LOOP` users.
- Acceptance:
  - `(loop for i upfrom 1 below 4 as c = (* i 10) while (< c 30) collect c)` returns `(10 20)`,
  - `maxima-try-load "../maxima/src/" "intl"` returns instead of failing or stalling,
  - no Maxima-local patching or loop-shape special casing is introduced.
- Risk:
  - additional `LOOP` clause-order bugs may surface immediately once `intl.lisp` completes.
- Effort: M

#### 1.4c Split loader APIs into authoritative fail-closed load and optional diagnostic enumeration
- Goal: G1, G2, G3
- Files: `lib/maxima-loader.lisp:44-151`, shared loader entrypoints used by scripts/tests/bench
- Depends on: 1.1, 1.3a, 1.4
- Work:
  - separate authoritative fail-closed Maxima loading from optional continue-on-error diagnostic scanning,
  - make authoritative loader entrypoints stop on first module failure and return structured failure with module id and condition,
  - make authoritative loader entrypoints reject caller-supplied policy overrides such as ad-hoc `:files`, `:source-dir`, `:habu-stop-on-error`, `:habu-reset-context`, `:habu-required-bindings`, or unknown keys unless they exactly match the declared authoritative loader policy,
  - derive authoritative `source-dir`, module set, and package-init directly from the sealed manifest snapshot on each invocation, or freeze/revalidate any mirrored loader globals before use and fail on divergence,
  - record exact authoritative loader identity in load results: manifest hash/id, normalized ordered module set, loader-policy identity, exact content identity of semantic bootstrap helpers, and a content fingerprint for the loaded upstream Maxima sources, not only success counts,
  - ensure failed authoritative loads restore package/load specials and do not expose partially initialized environments.
- Acceptance:
  - canonical script/test/bench entrypoints use only the fail-closed loader mode,
  - no authoritative path continues loading later modules after a module failure,
  - authoritative loads cannot drift through mutable mirror globals that diverge from the sealed manifest snapshot,
  - authoritative clean-state proofs require exact manifest/module-set, loader-policy, bootstrap-helper identity, and upstream Maxima source fingerprint match, not merely `ok == total`,
  - focused regressions prove failed loads leave no residual package/function/context mutations.
- Risk:
  - may invalidate current triage flows that rely on dirty partial environments.
- Effort: M

#### 1.5 Remove whole-file loader caps and make load scalable
- Goal: G1, G2
- Files: `src/interp/repl.zig:2096`
- Depends on: none
- Work:
  - remove 1 MiB whole-file cap,
  - move `load` to streaming or other unbounded semantics appropriate for real source files.
- Acceptance:
  - `load` handles Maxima files larger than current cap, including `~/Work/maxima/share/draw/wbd.lisp`.
- Risk:
  - interacts with reader buffering, source location tracking, and GC root lifetime.
- Effort: M

#### 1.6 Make loader evaluation stable under GC and long loads
- Goal: G1, G2
- Files: macro/literal/chunk/root registration in `src/interp/repl.zig`, `src/interp/vm.zig`, `src/runtime/gc.zig`
- Depends on: 0.6, 1.5
- Work:
  - audit long-load roots for forms, chunks, literals, macros, symbols, and JIT metadata,
  - remove stale-key/root corruption during large module loads.
  - replace linear tenured/LOS metadata scans in GC marking with address-indexed metadata so promotion-heavy defmacro loads do not degrade to O(n^2) mark work.
- Acceptance:
  - repeated large-load runs do not rely on name scans, stale pointers, or accidental stability.
  - promotion-heavy Maxima loads do not spend the majority of sampled time in `markTenuredObject` / linear tenure metadata scans.
- Risk:
  - failures may manifest as unrelated parser/compiler errors.
- Effort: L

#### 1.7 Add a dedicated reader-conformance stage for the real Maxima module set
- Goal: G1, G2, G3
- Files: `src/reader/parser.zig:179-190`, `src/reader/parser.zig:1233-1338`, `../maxima/src/nparse.lisp:42-43`, `../maxima/src/nparse.lisp:169-184`, `../maxima/src/float.lisp:92`, `../maxima/src/transs.lisp:99`
- Depends on: 1.2, 1.5
- Work:
  - validate feature-conditionals, ordinary macro characters, dispatch characters, read-time eval, dotted-pair skip semantics, and parser-facing Unicode/string behavior on actual Maxima sources.
  - make the active readtable's ordinary macro characters visible on the canonical load path instead of consulting only dispatch macro state.
- Acceptance:
  - real Maxima source modules named in the manifest parse without local source patches,
  - `intl.lisp` can install and use `_` via `set-macro-character` on the real load path.
- Risk:
  - reader bugs often appear later as false compiler/runtime failures.
- Effort: M

#### 1.7a Close generic `defstruct` parity on the clean-load path
- Goal: G1, G2, G3
- Files: `lib/stdlib.habu:6198-6289`, `src/runtime/primitives/clos.zig:209-239`, upstream users such as `../maxima/src/trans5.lisp:78`, `../maxima/share/affine/sparsemat.lisp:10-37`, `../maxima/src/numth.lisp:1810-1814`
- Depends on: 1.3
- Work:
  - move `defstruct` closure onto the clean-load critical path,
  - implement a distinct generic structure representation or structure-type tag path that does not alias vector/CLOS heuristics,
  - make runtime type/class predicates use canonical `COMMON-LISP` type symbols rather than current-package interning,
  - derive runtime slot metadata from the owning `defstruct` package symbol so package-local accessors and `slot-value` agree on canonical slot identity instead of silently interning slots into `COMMON-LISP-USER`,
  - implement options actually used upstream, including `:type list`, `:named`, `:print-function`, and BOA `:constructor` lambda-lists,
  - implement slot initform/default semantics, constructor defaulting, and multiple explicit constructor forms without silent option skips,
  - implement representation-correct readers, `setf` writers, copier behavior, and printer dispatch for each supported struct kind,
  - delete shape-based “vector slot 0 is a symbol” classification for structures/CLOS instances,
  - separate generic slot protocol from structure access so `slot-value`/related APIs only operate on true slot-bearing objects,
  - make `class-of` / `type-of` / `typep` / `subtypep` distinguish structures, vectors, and CLOS instances correctly, including the `structure-object` / `structure-class` lattice.
- Acceptance:
  - upstream `defstruct` forms load on the clean path with correct constructor/accessor/predicate/type behavior,
  - package-local `defstruct` accessors and `slot-value` resolve the same slot symbols across non-`COMMON-LISP-USER` packages,
  - slot defaults/initforms apply correctly,
  - `copy-structure` and writer paths follow the declared representation,
  - structure printing reaches the declared print function or correct default structure printer,
  - `:named` structures do not collide with current vector/CLOS identity heuristics,
  - plain vectors beginning with symbols are never misclassified as structures or instances,
  - runtime type/class predicates return canonical CL type symbols regardless of current package,
  - generic slot protocol rejects structures that are not true slot-bearing standard objects/conditions,
  - structure instances land in the correct runtime type/class lattice rather than generic vector/standard-object fallbacks.
- Risk:
  - touches object identity, type dispatch, copying, and printing.
- Effort: XL

#### 1.7b Remove borrowed-slice IR from compiler-generated call nodes
- Goal: G1, G2, G3
- Files: `src/compiler/compile.zig:19074-19080`, `src/compiler/ir.zig:1508-1518`, crash witness `../maxima/src/numth.lisp` form 151 `DEFSTRUCT (GF-DATA (:PRINT-FUNCTION ...))`
- Depends on: 1.7a
- Work:
  - make every compiler-generated `call`/`tailcall` own its arg slice instead of borrowing stack-backed arrays,
  - audit helper builders that bypass `IrBuilder.call` / `IrBuilder.tailcall`,
  - add a focused regression that proves `DEFSTRUCT` printer-install compilation survives specialization on the clean Maxima path.
- Acceptance:
  - canonical `tools/maxima-rtest.lisp rtest1` no longer dies with exit `132` / `EXC_BAD_ACCESS` in `p07c_specialize`,
  - no helper constructs `call`/`tailcall` nodes with borrowed arg slices.
- Risk:
  - the same bug class can silently corrupt other compiler-generated call sites until the helper cutover is complete.
- Effort: M

#### 1.8 Close current `defmfun-check` / later `mforma` blockers generically
- Goal: G1, G2
- Files: actual failing paths from current RCA dot, currently `../maxima/src/defmfun-check.lisp` and later `../maxima/src/mforma.lisp`, plus the related Habu reader/compiler/runtime sites proved by RCA
- Depends on: 1.4, 1.6, 1.7, 1.7a
- Work:
  - re-run current failing form under the cleaned loader path,
  - prove root cause in Habu semantics,
  - fix generically in reader/compiler/runtime/VM.
- Acceptance:
  - current `defmfun-check` blocker loads with no Maxima-local patch file,
  - the load path then continues through the next upstream blocker without reintroducing patch layers,
  - proof includes failing form, root cause, fix, and regression.
- Risk:
  - this may split into several independent core-language defects.
- Effort: L

### Phase 2 — Make Upstream Maxima Run

#### 2.1a0 Fix CL string constructor semantics on the canonical Maxima reader path
- Goal: G2, G3
- Files: `../maxima/tests/rtest6.mac:94-108`, `../maxima/src/nparse.lisp:324`, `lib/stdlib.habu:2365`, `src/compiler/compile.zig:18862`, and the Habu array/string constructor path used by `scan-string`
- Depends on: Phase 1
- Work:
  - preserve `:element-type` through `make-array` lowering and runtime construction on the Maxima reader path,
  - stop coercing 1-D `:fill-pointer` character arrays into generic vectors on the Maxima reader path,
  - make `stringp`, `length`, `copy-seq`, and related constructor semantics respect fill pointers and preserve string results for `scan-string`,
  - prove canonical `mread` returns strings, not vectors, for the `rtest6` front-door string forms.
- Acceptance:
  - canonical `test-batch` of `rtest6.mac:94-108` no longer materializes vectors where strings are required,
  - direct `mread` probes on the same path return strings for the `rtest6` string cases,
  - `array-element-type` and `make-array :element-type 'character` no longer erase string intent to `t` on the fill-pointer path,
  - fill-pointer-backed token buffers do not expose backing-store length or trailing garbage through `length` / `copy-seq`.
- Risk:
  - this bug can masquerade as later parser/operator failures.
- Effort: L

#### 2.1a1 Fix canonical `rtest6` dynamic operator/read-state semantics
- Goal: G2, G3
- Files: `../maxima/tests/rtest6.mac:103-145`, `../maxima/src/nparse.lisp:560-745`, `../maxima/src/nparse.lisp:1165-1235`, `../maxima/src/nparse.lisp:1638-1775`, `../maxima/src/comm.lisp:21-58`, Habu reader/load state in `src/reader/parser.zig`, `src/interp/repl.zig`, and package/property storage touched by operator definition lookup
- Depends on: 2.1a0
- Work:
  - make dynamic operator declarations installed by `infix`, `prefix`, `postfix`, `nary`, `matchfix`, and `nofix` visible to subsequent canonical reads in the same batch/test stream,
  - prove the scanned token is the exact symbol carrying `led` / `nud` / `lbp` / related parser properties across the `rtest6` setup block and following reads,
  - focus the fix on symbol/package/property/trie identity instead of unrelated display mappings such as `putopr` / `getopr`,
  - add focused canonical regressions for `infixie`, `naryie`, and `matchfixie` on the same load/batch path that `tools/maxima-rtest.lisp` uses,
  - prove the same operator/read-state fix also preserves built-in operator and list-bracket interaction for the immediate successor forms at `rtest6.mac:130`, `rtest6.mac:136`, `rtest6.mac:142`, and `rtest6.mac:145`.
- Acceptance:
  - focused proofs show the parser sees the same symbol identity that carries the operator parser properties for `infixie`, `naryie`, and `matchfixie`,
  - current `tools/maxima-rtest.lisp rtest6` provisional runner no longer fails at `../maxima/tests/rtest6.mac:110` with `infixie is not an infix operator`,
  - the same run reads and prints the custom-operator forms at `rtest6.mac:110-126` and the immediate built-in/list interaction forms at `rtest6.mac:130-145` without local patching or synthetic pre-registration,
  - once `2.3` and `2.4` land, the same proof is re-run on the canonical batch/test path.
- Risk:
  - the real remaining surface is symbol/package/property/trie identity, not stale buffered-token state or display-name mappings.
- Effort: L

#### 2.1b Fix canonical `rtest6` subscripted-callable/operator-expression semantics
- Goal: G2, G3
- Files: `../maxima/tests/rtest6.mac:1-145`, especially `rtest6.mac:36-63` and `rtest6.mac:103-145`, upstream callable/operator machinery in `../maxima/src/acall.lisp`, `../maxima/src/comm.lisp`, `../maxima/src/nparse.lisp:1219-1235`, and the Habu compiler/runtime call/function-designator/macroexpansion path in `src/compiler/compile.zig`, `src/interp/repl.zig`, `src/interp/vm.zig`
- Depends on: 2.1a0, 2.1a1
- Work:
  - implement the missing `mqapply` / subscripted-call surface end-to-end across reader, compiler, and runtime call paths,
  - fix CL function-designator semantics so symbols are not treated as functions merely by being symbols and `coerce` / `functionp` / callable dispatch reflect real `fdefinition` semantics,
  - close the real semantic gap behind the current `rtest6` wrong-answer cluster for `buildq`, `apply`, `map`, subscripted functions, and user-defined operator expressions,
  - preserve Maxima's raw nested callable structure for forms such as `f(x)(y)` and `f(x)(y)(z)` until the `mqapply`/operator path consumes it, instead of collapsing compound function positions into generic call IR too early,
  - prove whether failures come from callable-value lowering, subscripted function application, lambda/environment capture, operator-valued expression handling, or a shared evaluator/compiler bug,
  - add focused regressions for the exact upstream shapes before broadening to later Maxima files,
  - update the older `habu-fix-rtest6-macro-de33e2c1` execution plan so it matches the real canonical failure surface instead of the earlier narrower guess.
- Acceptance:
  - Maxima forms carried through `mqapply` no longer die at a missing subsystem boundary,
  - ordinary symbols no longer satisfy `functionp` unless they are true function designators under CL semantics,
  - canonical `tools/maxima-rtest.lisp rtest6` gets past the current early wrong-answer cluster for the upstream forms at `rtest6.mac:36-63` and `rtest6.mac:103-145`,
  - the raw symbolic `op` / `args` assertions for `f(x)(y)` and `f(x)(y)(z)` at `rtest6.mac:45` and `rtest6.mac:57` pass before and after evaluation,
  - no Maxima-local semantic patch or special-case is introduced,
  - the residual failure, if any, is a later concrete Habu gap with new evidence.
- Risk:
  - several visible wrong answers may collapse to one shared callable/environment bug, or they may split into independent compiler/runtime defects.
- Effort: XL

#### 2.1c Run `rtest6b` and the immediate successor core slice before broad sweeps
- Goal: G2, G3
- Files: `../maxima/tests/rtest6.mac:157-230`, `../maxima/tests/rtest6b.mac:1-182`, especially `rtest6b.mac:42`, `rtest6b.mac:50`, `rtest6b.mac:89`, `rtest6b.mac:104`, `rtest6b.mac:109`, `rtest6b.mac:117-182`, the next canonical `testsuite.lisp` entries reached after `rtest6`, `tools/maxima-rtest.lisp`, and the concrete Habu subsystem files exposed by those runs
- Depends on: 2.1b
- Work:
  - finish the remaining contiguous `rtest6` front-door blocks for string formatting, `parse_string` float readback, and string-driven operator/substitution semantics before leaving the file,
  - run `rtest6b` on the same canonical path immediately after `rtest6` is clean enough to advance,
  - keep converting the next concrete break/diff/hang into grounded Habu bugs until the run stops finding front-line parser/callable/runtime floor defects,
  - treat the immediate successor proofs for subscripted-callable substitution and mutable operator/string rendering as part of the same front-door floor, not a later broad sweep,
  - extend that concrete `rtest6b` successor slice through the contiguous `simp:false` TeX/operator rendering block and its immediate state-restoration checks,
  - refuse to jump ahead to broad sweeps while the immediate successor slice is still failing for basic language/runtime reasons.
- Acceptance:
  - the exact upstream `rtest6.mac:157-230` string/readback/string-operator checks are either passing or reduced to new grounded Habu bugs before `rtest6b` closure is claimed,
  - canonical `tools/maxima-rtest.lisp rtest6b` runs and the next immediate successor file is classified without patch layers,
  - the exact upstream successor checks at `rtest6b.mac:42`, `rtest6b.mac:50`, `rtest6b.mac:89`, `rtest6b.mac:104`, `rtest6b.mac:109`, and `rtest6b.mac:117-182` are either passing or reduced to new grounded Habu bugs before tranche sweeps begin,
  - mutable TeX/render state is proven to restore correctly on the canonical path after the `simp:false` / `texput` block,
  - remaining failures are no longer “front-door” parser/callable/runtime floor bugs hiding the rest of the suite.
- Risk:
  - `rtest6b` may expose a different but adjacent evaluator/runtime floor that needs to be closed before `test-batch`-level closure is meaningful.
- Effort: L

#### 2.1 Prove interactive and scripted execution on the clean load path
- Goal: G2
- Files: shared manifest/loader entrypoints plus script runner used by direct Habu execution, including `src/main.zig:47-102`
- Depends on: 2.1c
- Work:
  - re-run interactive and scripted proof only after the front-door `rtest6` / `rtest6b` floor defects move,
  - run a script that loads Maxima and evaluates representative simplify/factor/solve/integrate forms,
  - make top-level process exit status fail closed on fatal script/load errors,
  - prove package/context state survives repeated evaluation.
- Acceptance:
  - interactive and scripted sessions produce stable, repeatable results after the front-door `rtest6` / `rtest6b` blockers are removed,
  - direct `habu` process execution exits non-zero on fatal script/load errors,
  - script/bootstrap entrypoints use canonical repo-root/trusted-root helper resolution and explicit failure on undeclared ambient script paths.
- Risk:
  - hidden global/package state corruption often appears only on repeated runs.
- Effort: M

#### 2.2 Close condition/restart/unwind semantics used by real Maxima paths
- Goal: G2, G3
- Files: `src/runtime/primitives/condition.zig:1-166`, `src/runtime/heap.zig:2936-2944`, condition/unwind machinery in `src/interp/vm.zig`, relevant runtime entry points, `src/runtime/primitives/clos.zig:30-42`, `src/runtime/primitives/clos.zig:112-156`, `src/runtime/primitives/clos.zig:160-289`, `lib/stdlib.habu:474`, `lib/stdlib.habu:3093-3116`, `lib/stdlib.habu:3191`, `lib/stdlib.habu:6649`, `lib/stdlib.habu:7437-7557`
- Depends on: 0.9, 2.1a, 2.1b, 2.1c
- Work:
  - implement actual handler dispatch,
  - implement restart lookup/invocation,
  - remove Lisp-level condition/restart stubs and no-op wrappers such as raw `throw '%condition%` signaling and no-op `with-condition-restarts`,
  - implement typed `error` / `cerror` condition construction semantics instead of flattening to string `%error` signaling,
  - make runtime condition mapping preserve the correct condition classes, including slot-related conditions such as `UNBOUND-SLOT`,
  - make VM-raised standard conditions use canonical `COMMON-LISP` condition symbols instead of current-package or ad-hoc interned identities,
  - make runtime-generated standard conditions carry truthful slot payloads for the actual fault object (`pathname`, `stream`, `package`, `instance`, and peers) instead of nil-filled placeholders,
  - audit and eliminate raw VM `doThrow(sym_condition_tag, ...)` bypass sites so VM-raised `package-error`, `stream-error`, `file-error`, `type-error`, and peers go through the canonical condition-construction path,
  - unify or remove the separate boxed `Condition` object representation so primitive-created conditions, `make-condition`, and VM-raised conditions share truthful class/slot semantics,
  - make the generic slot protocol (`slot-value`, `(setf slot-value)`, `slot-exists-p`, `slot-boundp`, `slot-makunbound`) truthful on condition objects or remove any non-canonical condition representation that cannot satisfy it,
  - preserve `define-condition` option semantics and metadata such as `:report` and `:documentation` so user-defined conditions keep truthful debugger/reflection behavior,
  - close the remaining advertised standard-condition accessor surface on runtime/VM conditions, including `cell-error-name`, `unbound-slot-instance`, and `print-not-readable-object`,
  - remove condition accessor error masking so malformed runtime conditions do not collapse into silent `nil` results,
  - make `break`, `warn`, load-abort, catch/throw, block/return-from, tagbody/go, and debugger-facing flows truthful.
- Acceptance:
  - no advertised restart path is stubbed,
  - Lisp-level condition helpers route through the same real handler/restart machinery as VM/runtime signaling,
  - typed `error` / `cerror` calls preserve condition class and initargs through handlers,
  - runtime-raised condition classes match the actual fault path rather than collapsing to unrelated generic condition types,
  - VM-raised standard conditions carry canonical `COMMON-LISP` condition identity and the real offending object in their fault-specific slots,
  - direct accessor proofs for `package-error-package`, `stream-error-stream`, `file-error-pathname`, `type-error-datum`, and `type-error-expected-type` pass on VM-raised conditions,
  - `class-of`, `condition-type`, `condition-slot`, and simple-condition format accessors stay truthful on primitive-created and VM-created conditions alike,
  - reflective slot protocol operations, including `(setf slot-value ...)`, on condition objects do not reject or lie because of a separate boxed condition representation,
  - the full advertised standard-condition accessor surface, including `cell-error-name`, `unbound-slot-instance`, and `print-not-readable-object`, stays truthful on runtime/VM conditions,
  - user-defined conditions preserve `define-condition` report/documentation metadata through debugger/reflection paths,
  - condition accessors do not silently hide malformed runtime condition state,
  - Maxima runtime paths depending on restarts/NLX behave correctly.
- Risk:
  - touches core control-flow invariants.
- Effort: XL

#### 2.3 Fix stream and pathname semantics used by batch/test-batch/share runtime
- Goal: G2, G3
- Files: `src/runtime/primitives/io.zig:1687-1725`, stream machinery in `src/runtime/primitives/io.zig`, `src/interp/vm.zig:1834-1843`, `src/interp/vm.zig:5342-5348`, `lib/stdlib.habu` pathname/open helpers including logical-pathname helpers, `../maxima/src/mload.lisp:50-73`, `../maxima/src/mload.lisp:172-205`, `../maxima/src/macdes.lisp:28-86`
- Depends on: 1.1, 1.5, 1.7
- Work:
  - implement correct `make-string-input-stream` `start`/`end` slice semantics,
  - remove fixed-size coercion buffers and large-input rejection,
  - hard-cut over to one canonical stream/path primitive surface and delete or rewire duplicate legacy entrypoints that bypass pathname-aware semantics,
  - finish two-way/echo/synonym/file stream positioning behavior,
  - implement true synonym-stream behavior via symbol indirection, dynamic rebinding, and full I/O/query/control delegation,
  - implement required query/introspection delegation for composite streams,
  - make stream/path query results use canonical `COMMON-LISP` type/designator symbols,
  - make `probe-file` / `truename` semantics canonical and truthful at the primitive path layer, with distinct CL failure contracts,
  - close pathname object algebra and component fidelity: roundtrip semantics for `namestring`/`parse-namestring`, canonical `merge-pathnames` composition, component accessors, wildcard predicates, and homedir/host handling,
  - make `#p` reader literals allocate pathname objects at read time instead of leaking `(parse-namestring ...)` forms into constants and defaults,
  - use one canonical pathname-designator serialization path across `namestring`, `truename`, directory creation, and filesystem primitives,
  - make primitive `open`/file-create/designator operations consume canonical pathname designators instead of raw cwd-relative strings,
  - bring ordinary file query/read/write helpers under the same canonical/trusted-root contract as `open`,
  - implement CL `open` disposition semantics (`if-exists` / `if-does-not-exist`) correctly and fail closed,
  - bring delete/rename path mutation primitives under the same canonical/trusted-root rules as open/create,
  - bring directory-creation pathname primitives such as `ensure-directories-exist` under the same canonical/trusted-root rules as file opens,
  - close canonical `directory` / wildcard search / `pathname-match-p` semantics under pathname designators, case rules, and trusted-root/no-cwd behavior,
  - derive/protect stream truenames for file-backed streams,
  - propagate correct `$load_pathname` / `*LOAD-PATHNAME*` context through `batch-stream` and `batchload-stream`,
  - implement real `logical-pathname`, `translate-pathname`, and `translate-logical-pathname` semantics instead of stubs/identity coercions,
  - make pathname designators and search semantics correct for batch/test-batch.
- Acceptance:
  - Maxima code using `(make-string-input-stream str start end)` works,
  - every VM opcode and primitive-facing stream/path operation routes through the canonical cleaned implementation,
  - synonym streams dereference the current symbol value and delegate the full supported stream surface through it,
  - composite streams implement the required query/introspection surface by correct delegation,
  - stream/path query primitives return canonical CL type/designator symbols regardless of current package,
  - `probe-file` / `truename` results are canonical and not just echoed input designators,
  - `truename` and `probe-file` do not silently collapse into each other and do not rely on ambient cwd/default resolution,
  - pathname objects preserve component fidelity and roundtrip semantics rather than lossy ad hoc string forms,
  - `parse-namestring` and `merge-pathnames` implement canonical composition semantics rather than ad hoc Unix-string concatenation,
  - filesystem primitives do not rely on divergent lossy pathname serializers,
  - primitive file open/create paths reject ambient cwd-relative resolution and preserve canonical pathname identity end-to-end,
  - ordinary file query/read/write helpers obey the same canonical pathname and trusted-root rules as `open`,
  - primitive write/create helpers, including directory creation, obey the trusted-root/canonical-designator contract,
  - delete/rename helpers obey the same trusted-root/canonical-designator contract,
  - directory search and pathname matching do not depend on ambient cwd or simplified ad hoc matching,
  - directory results themselves are canonical/truthful pathname objects rather than raw relative scan output,
  - `open` honors `if-exists` / `if-does-not-exist` semantics instead of silently creating or truncating,
  - file-backed batch/test-batch streams preserve truthful truename and load-path context,
  - batch/test-batch stream paths run without custom stream hacks.
- Risk:
  - stream bugs can masquerade as parser or test-harness failures.
- Effort: XL

#### 2.4 Make `tools/maxima-rtest.lisp` canonical and fail-closed
- Goal: G2, G3
- Files: `tools/maxima-rtest.lisp:14-15`, `tools/maxima-rtest.lisp:42-48`, `tools/maxima-rtest.lisp:65-83`
- Depends on: 1.1, 1.4c, 2.1, 2.3
- Work:
  - remove path guessing and swallowed `file_search` failures,
  - remove local testsuite registry decoding/parsing as a second correctness authority and route test selection plus expected-failure extraction through canonical Maxima-side helpers or one upstream-compatible decoder,
  - collapse the separate preflight testsuite bootstrap path onto the same authoritative loader/bootstrap contract used by the real runner,
  - refuse to run any rtest unless the shared loader manifest completed cleanly with zero missing/failed modules,
  - validate requested test names,
  - confine resolution to declared testsuite roots,
  - require the underlying `habu` process to exit non-zero on fatal startup/script/load failures so automation outcomes are actually fail-closed,
  - treat upstream warning-only suite states such as unknown-test selection, empty test sets, share mismatch, and accumulated suite errors as structured failure for authoritative automation,
  - convert break/diff/unexpected-pass outcomes into explicit automation failure.
- Acceptance:
  - tool either resolves via canonical Maxima file search or fails explicitly,
  - no `../` escape or fabricated path fallback remains,
  - no local testsuite-entry decoder can drift from `$testsuite_files` / `$share_testsuite_files`,
  - test-name selection and expected-failure extraction do not rely on a divergent pre-loader registry bootstrap path,
  - fatal startup/script/load failures from the underlying `habu` process cannot be misreported as success by the runner,
  - upstream warning-only states cannot be misreported as a clean authoritative pass,
  - dirty/partial loader state, breaks, diffs, and unexpected passes produce non-success automation outcome.
- Risk:
  - current “working” invocations may be depending on guessed paths.
- Effort: M

#### 2.5 Bring `defint.lisp` and `residu.lisp` into the clean scope
- Goal: G2
- Files: current failing upstream modules and the Habu subsystem proven by RCA
- Depends on: 2.2, 2.3
- Work:
  - rerun both modules after earlier semantic cleanup,
  - fix the actual blocker,
  - remove any exclusion state.
- Acceptance:
  - both modules load on the clean path with focused regressions.
- Risk:
  - likely exercises control flow, numeric tower, or macro/reader edges.
- Effort: L

### Phase 3 — Canonical Correctness Closure

#### 3.1 Make the upstream testsuite pipeline the single correctness authority
- Goal: G3
- Files: `../maxima/src/mload.lisp:379-960`, `tools/maxima-rtest.lisp`, any custom runner glue
- Depends on: 2.1, 2.2, 2.3, 2.4
- Work:
  - remove or demote custom correctness authorities,
  - ensure the upstream `run_testsuite` / `test-batch` pipeline is what automation uses,
  - require authoritative script/test/bench entrypoints to use stop-on-first-failure loader mode and never continue loading later modules after a module failure,
  - require authoritative automation to depend on process-level nonzero exit for fatal startup/script/load failures before any parsed child output is treated as authoritative,
  - require unknown-test, empty-selection, share-mismatch, and accumulated-suite-error states from the upstream testsuite path to be surfaced as structured failure before wrapper demotion,
  - make the canonical runner fail closed on partial loader state and non-success test outcomes.
- Acceptance:
  - correctness claims cite upstream testsuite pipeline runs, not ad-hoc loader scripts,
  - authoritative entrypoints do not expose dirty partial loader state before failure is reported,
  - parsed runner output from a child process that exited non-zero or was signaled is never treated as a clean correctness result,
  - warning-only upstream suite states are either converted into structured failure or remain explicitly wrapped by a fail-closed authoritative layer,
  - automation cannot report success if loader state is partial, or if tests break, diff, or unexpectedly pass.
- Risk:
  - exposes missing stream and answer-file semantics.
- Effort: M

#### 3.2 Close `rtest1` on the clean path
- Goal: G3
- Files: `../maxima/tests/rtest1.mac`, failing Habu subsystems found during run
- Depends on: 3.1
- Work:
  - run, classify, and close each remaining failure without patch layers.
- Acceptance:
  - no hangs, no infrastructure failures, no patch-dependent passes.
- Risk:
  - early rtests cover a wide semantic surface.
- Effort: XL

#### 3.3 Sweep core testsuite files
- Goal: G3
- Files: `../maxima/tests/**`, manifest-driven runner inputs
- Depends on: 3.2
- Work:
  - run each core file,
  - classify pass/fail/hang/language-gap,
  - after per-file grounding, run ordered multi-file core tranches through upstream `$run_testsuite` itself to prove file ordering, suite aggregation, and carried runtime state on the canonical path,
  - require a literal upstream default core invocation shape (`run_testsuite()`) so the exact upstream default core list, answer-file/query-stream defaults, and ordering are proven, not only curated subsets,
  - open focused follow-up items only for concrete gaps.
- Acceptance:
  - core closure includes authoritative ordered tranche runs through upstream `$run_testsuite`, not only individually clean files,
  - core closure includes an upstream default invocation with defaults intact, not only caller-supplied subsets or non-default option overrides,
  - no vague “Maxima issue” bucket remains for core tests.
- Risk:
  - requires discipline to keep failures grounded and deduped.
- Effort: XL

#### 3.4 Sweep share tests and share module execution
- Goal: G2, G3
- Files: `../maxima/share/**`
- Depends on: 1.5, 1.7, 2.3, 3.2
- Work:
  - exercise upstream `run_testsuite` share-mode switching (`share_tests=t` and `share_tests=$only`) as part of authoritative share proof, not only per-file share runs,
  - run share tests and representative share packages,
  - include upstream package-local share suite setup flows that install their own suite selection/search roots before calling `run_testsuite()` (for example share packages that ship `setup_tests.mac`-style entrypoints outside the default registries), and prove both the literal default `run_testsuite()` shape and upstream-documented package-local `tests=` subset invocation after that setup,
  - classify failures by real Habu feature gap,
  - after per-file grounding, run ordered share tranches through upstream `$run_testsuite` share mode to prove list composition, search-path handling, aggregation, and carried runtime state on the canonical path,
  - require literal upstream share-mode invocation shapes with defaults intact: `$run_testsuite(share_tests=$only)` for share-only defaults and `$run_testsuite(share_tests=t)` for the combined core+share default path,
  - treat missing load/search/runtime semantics as Habu bugs, not optional misses.
- Acceptance:
  - share-suite authority includes upstream share-mode list composition and search-path semantics, not only manually selected share files,
  - share closure includes upstream package-local suite-setup flows that rebind suite lists/search roots before both a literal default `run_testsuite()` call and documented package-local `tests=` subset calls,
  - share closure includes authoritative ordered tranche runs through upstream `$run_testsuite`, not only individually clean share files,
  - share closure includes upstream default invocation shapes with defaults intact for both share-only and combined core+share modes, not only caller-supplied subsets or non-default option overrides,
  - share failures are reduced to concrete subsystem gaps with evidence.
- Risk:
  - share tree is where loader caps, string-stream semantics, and path/search bugs surface first.
- Effort: XL

#### 3.5 Publish the real remaining language-gap list
- Goal: G3
- Files: generated from Phase 3 evidence
- Depends on: 3.3, 3.4
- Work:
  - collapse residual failures into concrete buckets: reader, package, compiler, conditions/NLX, numeric tower, CLOS/MOP, pathname/streams, GC/JIT interaction.
- Acceptance:
  - no residual “Maxima-specific” bucket unless it is proven upstream-specific.
- Risk:
  - requires hard dedupe discipline.
- Effort: M

### Phase 4 — Truthful Performance Only After Clean Correctness

#### 4.1 Make benchmark bring-up fail-closed
- Goal: G4
- Files: `bench/maxima_workload.zig:394-401`, `bench/maxima_workload.zig:763-890`
- Depends on: Phase 3
- Work:
  - benchmark harness must load the same clean environment as scripts/tests,
  - abort on loader failures, missing bindings, patch dependence, or fake FASL/source fallback,
  - require hermetic trusted-root selection,
  - record full authoritative loader identity in benchmark output, including loader-policy, bootstrap-helper identity, and upstream Maxima source fingerprint, not only root/module provenance,
  - require benchmark clean-state checks to include exact manifest identity and normalized module set, not only aggregate counts,
  - require benchmark startup/setup failure to terminate the process non-zero instead of returning normal output,
  - invalidate the run if any selected workload fails during setup, warmup, or timed execution.
- Acceptance:
  - no timing output is emitted from a dirty or partial Maxima environment,
  - no benchmark result is emitted for a selected set unless every selected workload succeeds,
  - a subset or caller-overridden module list cannot satisfy authoritative clean-state checks,
  - failed benchmark startup/setup cannot be summarized as a successful run with partial metadata,
  - benchmark output records the full authoritative loader identity needed to prove sealed canonical bootstrap and the exact upstream Maxima tree content used.
- Risk:
  - current perf numbers may disappear until correctness is real.
- Effort: M

#### 4.1a Make performance and validation tooling hermetic and fail-closed
- Goal: G4
- Files: `tools/maxima-hotspots`, `tools/validate-session`, `tools/perf-loop`, `tools/gc-compare`, `tools/perf-test`, `tools/test-all`, `tools/jit-bench`, `bench/check.zig`, `bench/maxima_workload.lisp`, `bench/comprehensive_bench.zig`, `tools/bench_pack_runner.py`, `tools/comprehensive-bench`, `tools/bench_compare.sh`, `tools/maxima-bench`, `tools/run`, `tools/build`, `tools/build.lisp`, helper scripts they invoke
- Depends on: 4.1
- Work:
  - pin repo root and benchmark binary locations so tooling cannot drift by cwd,
  - ban `/tmp` or other ambient executable proof scripts from correctness/perf gates,
  - make downstream tooling reject `ERR(...)` / zero-ns / partial-loader benchmark output instead of summarizing it,
  - make history/ranking/action generation fail closed by default when Maxima exec or workload truthfulness gates fail,
  - require support tooling to use the same canonical workload manifest and trusted-root contract as the benchmark binary,
  - require binary/helper provenance recording so tooling reports show which repo-root binaries and helpers were used,
  - require helper APIs and interchange formats to surface dirty workload status as fatal at the top level, not only per-row,
  - require comparator completeness: no geomean, hotspot, GC, or perf summary from a biased surviving subset of declared workloads,
  - require downstream validation/report tooling to validate the full authoritative loader identity carried by benchmark payloads: manifest/module-set, loader-policy identity, and bootstrap-helper identity must exactly match the sealed canonical bootstrap,
  - require workload-attributed, identity-grounded JIT coverage in support tooling rather than coarse compiled-count proxies,
  - require recommendation output to preserve exact dot provenance or emit no recommended dot,
  - treat benchmark payloads as untrusted data in all helper/report paths; parse from files/stdin/JSON instead of code interpolation,
  - require explicit provenance and policy for any environment-supplied peer-runtime benchmark commands,
  - make malformed/incomplete benchmark payloads fatal before baselines or reports are saved,
  - require strict helper payload schema/version/engine/workload identity validation before any result is treated as authoritative,
  - require authoritative validation helpers to prove payload provenance from the expected repo-pinned producer binary,
  - require build/prepare steps for authoritative helpers to succeed before any benchmark/helper output may be published or saved,
  - require producer commands themselves to exit successfully; parsed payload from a failed child is not authoritative,
  - make perf wrappers such as `tools/maxima-hotspots` fail on dirty loader/workload metadata even if perf-only thresholds pass,
  - make stale-binary paths impossible in authoritative helpers such as `tools/perf-test`; failed build/prepare steps must prevent baseline update or report generation,
  - replace ambient benchmark/validation proof scripts such as hardcoded `../maxima/**` and `/tmp/**` executables with repo-pinned manifest-backed paths,
  - make authoritative peer-runtime/build entrypoints disable host init/config injection (`--no-userinit`, `--no-sysinit`, ASDF/source-registry neutralization, repo-pinned transitive system roots),
  - validate authoritative bootstrap source inputs before any `load` / `with-open-file` / ASDF resolution: repo-owned regular files only, with no symlink/path-traversal/non-regular escape,
  - require every default external executable used by authoritative helpers (`zig`, `sbcl`, `python3`, `timeout`, peers) to resolve through an allowlisted absolute path with captured provenance instead of ambient `PATH` lookup,
  - make authoritative wrapper interpreters themselves start in isolated mode with hostile startup hooks disabled or sanitized (`BASH_ENV`, `ENV`, `PYTHONPATH`, `PYTHONHOME`, `sitecustomize`, shell rc hooks, and peers),
  - require authoritative Python wrappers to import adjacent helper modules by verified repo-local file identity, not ambient `sys.path` / bare import resolution,
  - forbid authoritative helper entrypoints from relying on `/usr/bin/env` or other launcher-side `PATH` resolution in their own shebang/interpreter selection unless they are explicitly demoted from authoritative use,
  - pin non-ASDF transitive bootstrap `load` / `with-open-file` paths inside canonical helpers such as `tools/build.lisp` to script-truename/repo-root resolution or explicitly demote direct helper invocation,
  - make standalone benchmark binaries such as `bench/comprehensive_bench.zig` fail closed themselves, or explicitly demote them to non-authoritative internal use,
  - require authoritative helpers to run under an explicit allowlisted environment contract and capture effective environment provenance in saved outputs,
  - require saved baselines/reports/comparisons to capture and validate external runtime/tool identity, helper/repo content identity, and producing tree state: resolved path, version, relevant invocation flags, effective environment contract, repo revision, helper content fingerprint, and clean/dirty status,
  - require published JSON/report provenance to use a redacted structured view; no raw child `stdout`/`stderr`, full command vectors, secrets, or host-local paths may be emitted in authoritative artifacts,
  - require authoritative build artifact outputs to be validated before write with the same repo-pinned regular-file, no-symlink, no path-traversal, no non-regular sink contract,
  - require publication sinks for baselines/history/reports to be validated before write: repo-pinned regular files only, with symlink/path-traversal/non-regular rejection for both default and caller-supplied paths,
  - require baseline/history/report inputs to be validated before read with the same repo-pinned regular-file, no-symlink, no-device/FIFO, no path-traversal contract,
  - require workload-manifest/corpus inputs that drive completeness/publication gates to be validated before read with the same repo-owned regular-file, no-symlink, no path-traversal/non-regular contract,
  - require authoritative snapshot/history publication to be atomic and single-writer serialized: temp-file `fsync`+replace for JSON snapshots and locked single-record append for JSONL/history files,
  - require authoritative bootstrap/input/output path enforcement to be race-free at open/load/write time, using descriptor-anchored or equivalent semantics that prevent symlink/path swaps between validation and use,
  - require generic wrapper tools either to enforce repo-local allowlisted executable policy with provenance capture or to be explicitly demoted from authoritative workflows,
  - require one canonical supported build entrypoint; remove or demote legacy alternate build wrappers outside the cutover/provenance policy,
  - remove or demote legacy comparison/publishing entrypoints that cannot enforce the canonical gate/provenance rules.
- Acceptance:
  - hotspot/validation/perf-loop tooling only consumes hermetic repo-pinned binaries and scripts,
  - no downstream report, history entry, or prioritization output is produced from dirty benchmark runs,
  - no proof gate depends on mutable `/tmp` scripts or cwd-relative binaries,
  - authoritative bootstrap helpers never execute or read symlinked, non-regular, or out-of-repo source inputs,
  - `gc-compare` and `bench/check` cannot emit authoritative clean output from cwd-relative or non-provenance-tracked binaries,
  - `bench_pack_runner.py` top-level status/provenance reflect normalized workload completeness, not raw process success,
  - malformed, partial, or environment-spoofed comparator payloads cannot be saved or published as clean baselines,
  - helper payloads are rejected unless schema, producer identity, and provenance match the expected canonical producers,
  - downstream validation/report tooling rejects payloads whose authoritative loader identity, including upstream Maxima source fingerprint, diverges from the sealed canonical bootstrap,
  - authoritative helpers cannot run on stale binaries after failed build/prepare steps,
  - authoritative helpers reject parsed payloads from non-zero/signaled producer commands,
  - `tools/maxima-hotspots` cannot emit success from a perf-only gate when loader/workload truthfulness is dirty,
  - `tools/perf-test` cannot reuse stale binaries or update baselines after a failed build,
  - authoritative build/bootstrap flows cannot be influenced by host SBCL init files or external ASDF/source-registry state,
  - default external tools in authoritative workflows are never taken from ambient `PATH` without allowlist/provenance checks,
  - authoritative wrapper scripts cannot execute under hostile shell/Python startup injection before their own policy checks run,
  - authoritative Python wrappers cannot resolve helper modules from ambient import paths or symlink-shifted `sys.path[0]`,
  - authoritative helper entrypoints themselves do not select their interpreter through `/usr/bin/env` or ambient `PATH`,
  - direct helper execution such as `sbcl --load tools/build.lisp` is either repo-root-pinned and safe or explicitly non-authoritative,
  - authoritative build helpers reject caller-selected artifact sinks that are symlinked, non-regular, or out-of-repo before writing,
  - standalone benchmark binaries cannot publish partial `ns=0` / filtered-subset results as authoritative clean output,
  - authoritative published artifacts expose only redacted structured provenance, never raw child output or full secret-bearing command details,
  - saved reports/baselines record and validate external runtime/tool, environment, helper-code, and repo-state provenance before reuse or publication,
  - authoritative publication helpers reject symlinked, non-regular, or out-of-repo output sinks before writing,
  - authoritative baseline/history/report consumers reject symlinked, non-regular, or out-of-repo input sources before reading,
  - authoritative workload-manifest/corpus consumers reject symlinked, non-regular, or out-of-repo inputs before deriving declared workload identities,
  - authoritative bootstrap/input/output path protections remain sound under concurrent path mutation; validation cannot be bypassed by check/use races,
  - authoritative readers never observe torn/truncated/interleaved baseline/history artifacts from in-flight saves,
  - generic wrapper helpers cannot silently execute non-repo or non-allowlisted producers inside authoritative workflows,
  - there is one canonical supported build entrypoint; legacy build wrappers are either removed or explicitly non-authoritative,
  - summaries and comparisons are emitted only for the full declared workload set unless explicitly marked internal/micro-only,
  - shipped comparison helpers either hard-fail on missing peer runtimes/incomplete sets or are clearly demoted to non-authoritative internal use,
  - tooling JIT gates are grounded in the declared workload identities, not wrapper-only/global counters.
- Risk:
  - current local workflows may rely on loose tool invocation semantics.
- Effort: L

#### 4.2 Make JIT candidate selection and reporting truthful
- Goal: G4
- Files: `src/jit/candidates.zig:28-31`, `src/jit/candidates.zig:106-159`, `src/jit/candidates.zig:289-307`, `src/jit/candidates.zig:610-630`, `src/jit/backend.zig:1677-1830`, `src/jit/backend.zig:1964-2002`, `src/jit/backend.zig:2248-2515`, `src/jit/backend.zig:5493-5493`, `src/jit/backend.zig:6013-6013`, `src/jit/backend.zig:11462-11462`, `src/jit/literal_roots.zig:18-39`, `src/interp/vm.zig:612-612`, `src/interp/vm.zig:1706-1743`, `src/testing/compile_chunk.zig:344-408`, `src/testing/compile_chunk.zig:489-509`, `src/interp/repl.zig:3066-3135`, `src/interp/repl.zig:3238-3274`, `bench/comprehensive_bench.zig:153-153`
- Depends on: 4.1
- Work:
  - replace case-insensitive/package-stripping candidate matching with canonical identity-based matching,
  - stop silent skip/no-match behavior in compile helpers,
  - unify helper and runtime hoist admission/compile/status paths so test/bench helpers cannot diverge from product behavior,
  - make backend cross-call inlining, self-call detection, and primitive-call classification use canonical package-correct identity instead of bare-name/package-stripped fallback,
  - make JIT compile-status cache keys package/identity-correct so package-distinct chunks cannot share `.compiled` / `.unsupported` / `.failed` status by printed-name collisions,
  - make JIT status-cache invalidation/retry semantics sensitive to external hoist prerequisites such as literal roots, package/bootstrap state, global refs, and known-function sets, not only chunk identity,
  - make JIT admission, bridge invocation, and reporting reflect the real callable ABI surface, including rejecting or correctly supporting every fixed-arity ceiling on compiled entry, helper calls, indirect calls, recursive/TCO calls, and backend internal state,
  - either support or reject backend lowering for fixed arities beyond the backend's own entry-block parameter ceiling; no hidden >16 shape may compile into undefined behavior,
  - fix the known nested-self-call `call_indirect` regalloc crash class or explicitly remove those unsafe shapes from authoritative JIT benches and reports until the backend can execute them correctly,
  - make backend non-arity state ceilings such as loop phi/init/update width fail closed or fully supported; no fixed-size internal buffer may compile into latent corruption,
  - make recursive TCO continuation-stack depth either supported with explicit bounds handling or rejected fail-closed; no fixed continuation buffer may overflow silently,
  - classify non-IR unsupported backend shapes such as call-target/arity/loop-width ceilings as truthful `.unsupported` cases, not generic `.failed`, and keep cache/report semantics aligned with that classification,
  - make backend unsupported-tag and literal-root coverage accounting authoritative for JIT coverage reporting,
  - report eligible/skipped/unsupported coverage explicitly.
- Acceptance:
  - JIT compile/skip metrics are identity-based and auditable,
  - helper/runtime JIT reporting and admission behavior match,
  - backend cross-call/primitive resolution cannot alias same-spelling symbols across packages,
  - compile-status cache identity cannot collide across package-distinct chunks with same printed names,
  - cached `.unsupported` / `.failed` statuses do not survive changes in external hoist prerequisites that make the same chunk newly compilable,
  - compiled functions and internal JIT call paths do not silently compile or execute beyond any supported arity ceiling,
  - backend lowering for fixed arities beyond its supported ceilings and for loop-state width beyond supported ceilings fails closed or is fully supported; it never compiles into latent corruption,
  - recursive TCO continuation depth has an explicit support/rejection contract and cannot overflow a fixed buffer silently,
  - unsupported backend ceilings contribute to truthful unsupported classification/caching/reporting instead of inflating generic failure counts,
  - authoritative JIT benchmarks do not include known-crashing nested-self-call shapes such as `tak` unless the backend bug is fixed,
  - backend/literal-root truth and published JIT coverage metrics agree,
  - unsupported-node distribution is accurate.
- Risk:
  - will reduce reported JIT coverage before it improves.
- Effort: L

#### 4.3 Define the real workload set
- Goal: G4
- Files: `bench/maxima_workload.zig`, `bench/maxima_workload.lisp`, benchmark scripts/manifests
- Depends on: 4.1, 4.2
- Work:
  - keep microbenches,
  - add real Maxima workloads for factor/ratsimp/integrate and one direct Todd-Coxeter style gate,
  - ensure same environment and logical work across Habu and SBCL.
- Acceptance:
  - every published perf claim maps to a declared workload and clean loader state.
- Risk:
  - workload drift can make comparisons meaningless.
- Effort: M

#### 4.4 Optimize interpreter, JIT coverage, and GC from measured clean workloads
- Goal: G4
- Files: hot-path files proven by profiling
- Depends on: 4.3
- Work:
  - optimize call/dispatch/allocation/stream overhead,
  - expand JIT only from measured unsupported-node distribution,
  - improve GC nursery/tenure/remembered-set/root scanning based on real Maxima behavior.
- Acceptance:
  - optimizations are justified by clean workload evidence and do not reintroduce fake semantics.
- Risk:
  - premature optimization before full truthfulness will hide real correctness debt.
- Effort: XL

#### 4.5 Compare to SBCL with aligned methodology
- Goal: G4
- Files: Habu and SBCL bench harnesses
- Depends on: 4.3, 4.4
- Work:
  - same workloads,
  - same logical operations,
  - explicit geomean and per-workload ratios,
  - include memory and GC behavior once correctness is stable.
- Acceptance:
  - published comparison is apples-to-apples and reproducible.
- Risk:
  - meaningless if any upstream path is still patched or dirty.
- Effort: M

## 6. Dot Decomposition Requirement

Before implementation, each numbered plan item above must be decomposed into leaf dots with:
- file paths,
- root cause,
- dependency links,
- acceptance proof,
- focused validation command or run.

No implementation starts until the leaf dot exists and is ready.

## 7. Review Notes

### Round 1 accepted Critical/Major findings now covered

- loader-side semantic fixups were broader than early patch injection,
- `lib/maxima-stubs.lisp` needed explicit deletion/reduction gate,
- trusted-root policy and ambient-root bans were missing,
- single authoritative Maxima manifest/root contract was missing,
- builtin alias cleanup and side-effect-free lookup APIs were missing,
- legacy tests for forbidden fallback semantics were missing from the plan,
- package bootstrap and reader auto-create removal were missing,
- package state canonicalization was missing,
- generic autoload/function lookup cleanup was missing,
- loader scalability beyond 1 MiB files was missing,
- `make-string-input-stream` slice/large-input work was missing,
- condition/restart closure gate was missing,
- canonical `maxima-rtest` path validation was missing,
- benchmark fail-closed and JIT-truthfulness gates were missing.

### Round 2 accepted Critical/Major findings now covered

- logical pathname translation loading needed trusted-root policy, not cwd/default execution,
- benchmark provenance and hermetic root selection were missing,
- canonical rtest automation needed loader-clean and non-success fail-closed gates,
- benchmark validity needed all selected workloads to succeed, not `ERR(...)` records plus output,
- builtin wrapper removal needed to include compiler-side callable synthesis and symbol-designator fallback,
- JIT truthfulness needed helper/runtime pipeline unification,
- reader `pkg:sym` versus `pkg::sym` semantics were missing,
- package-context silent repair/reset had to be forbidden explicitly,
- loader dynamic special-state needed a dedicated closure task,
- batch file-stream truename / `$load_pathname` propagation needed explicit coverage,
- generic `defstruct` parity needed to be on the critical path before share sweeps.

### Round 3 accepted Critical/Major findings now covered

- perf/validation tooling now has an explicit hermeticity and fail-closed plan surface,
- logical pathname translations now require fail-closed load behavior with no masked/cross-candidate fallback,
- raw relative loads now have to resolve through the trusted loader context or fail,
- package lookup now explicitly requires one canonical symbol identity from one canonical state,
- `defstruct` now blocks clean-load closure and explicitly covers structure identity, initforms/defaults, writer/copier semantics, and printer integration.

### Round 4 accepted Critical/Major findings now covered

- generic relative-load resolution now explicitly bans guessed alternate candidates and trusted-root escape,
- truthful `probe-file` / truename semantics are now called out at the primitive path layer,
- `defstruct` now explicitly removes shape-based vector/CLOS classification and requires the structure type/class lattice,
- perf/validation tooling now explicitly covers `gc-compare` and `bench/check`,
- downstream summaries/comparators now require full-set completeness, workload-attributed JIT coverage, provenance-tracked binaries, and exact dot provenance.

### Round 5 accepted Critical/Major findings now covered

- primitive file/path surfaces now explicitly require canonical pathname designators and no ambient cwd resolution,
- synonym-stream semantics are now explicitly part of the stream/path plan,
- structure/type closure now explicitly requires canonical CL type symbols and slot-protocol separation,
- bench-pack/comparison helpers now explicitly require fatal top-level dirty status, untrusted-payload parsing, command provenance policy, malformed-payload rejection, and legacy helper cutover/demotion.

### Round 6 accepted Critical/Major findings now covered

- primitive pathname closure now explicitly covers `open` disposition semantics, full synonym-stream delegation, directory-creation helpers, and distinct `probe-file`/`truename` contracts,
- helper/tooling closure now explicitly covers schema/version/identity validation, producer provenance checks, and build-step success before authoritative output.

### Round 7 accepted Critical/Major findings now covered

- stream/path closure now explicitly hard-cuts over duplicate primitive surfaces, adds delete/rename and wildcard/pathname-match semantics, and requires one canonical pathname serialization path,
- composite stream query/introspection semantics are now explicit,
- tooling closure now explicitly includes `tools/maxima-bench` and requires successful producer exit for authoritative helpers.

### Round 8 accepted Critical/Major findings now covered

- primitive file query/read/write helpers are now explicitly under the canonical pathname/trusted-root contract,
- directory results now explicitly have to be canonical/truthful pathname objects,
- tooling closure now explicitly includes `tools/run` and repo-local allowlisted producer policy for authoritative workflows.

### Round 9 accepted Critical/Major findings now covered

- tooling cutover now explicitly includes `tools/build` / `tools/build.lisp` and requires one canonical supported build entrypoint.

### Round 10 accepted Critical/Major findings now covered

- pathname object/component roundtrip fidelity is now explicit, not just filesystem-routing behavior,
- stream/path query results now explicitly require canonical CL symbol identity.

### Round 12 accepted Critical/Major findings now covered

- pathname parsing/composition now explicitly requires canonical `parse-namestring` / `merge-pathnames` semantics, not ad hoc Unix-string merging.

### Known open risk

- Removing the false-progress layers will likely make the current load path look worse before it gets better. That is expected and required.
