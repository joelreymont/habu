# PLAN — Generic Common Lisp Closure For Maxima

Last updated: 2026-04-03
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
- E6: `test-batch` is the correctness authority.
- E7: benchmark harness rejects dirty loader states and reports truthful JIT eligibility/coverage.

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
  - reclassify anything retained as bootstrap-only and prove it is non-semantic.
- Acceptance:
  - no load path calls `lib/maxima-early-patches.lisp`,
  - `maxima-loader` and post-load code contain no semantic override of upstream Maxima functions/macros,
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
- Files: `src/runtime/heap.zig:416-430`, `src/runtime/heap.zig:3274-3351`, `src/runtime/heap.zig:4684-4698`, `src/compiler/compile.zig:7168-7189`, `src/compiler/compile.zig:9436-9498`, `src/compiler/compile.zig:10843-10884`, `src/interp/repl.zig:1487-1541`, `src/interp/repl.zig:1742-1767`, `src/interp/vm.zig:2465-2498`
- Depends on: none
- Work:
  - delete legacy symbol-table fallback,
  - delete unqualified/`CL`/`COMMON-LISP`/`CL-USER` retry logic,
  - delete case/name/% alias fallback builtin resolution,
  - delete unqualified-first special/global lookup.
- Acceptance:
  - package-qualified and current-package semantics are the only lookup semantics,
  - builtin resolution is canonical by symbol identity only.
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

#### 0.7 Remove fake FASL success paths
- Goal: G1, G3
- Files: `src/interp/repl.zig:1945-1991`, `src/interp/repl.zig:2317-2338`
- Depends on: none
- Work:
  - delete sibling-source substitution,
  - make `.fasl` / `.hfasl` execute real semantics or fail explicitly.
- Acceptance:
  - a `.fasl` load never silently loads source,
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
- Files: `lib/maxima-loader.lisp:6-26`, `lib/maxima-loader.lisp:30-159`, `lib/maxima-post-load.lisp:214-330`, `tools/maxima-rtest.lisp:1-83`, `bench/maxima_workload.zig:373-401`, `lib/stdlib.habu:7319-7347`
- Depends on: 0.1, 0.2
- Work:
  - define one source of truth for Maxima root, module manifest, search roots, and autoload scope,
  - make loader/scripts/tests/bench share the same manifest,
  - ban writable ambient roots such as `/tmp/maxima/**` and user-home executable search roots such as `~/.maxima/**`,
  - ban cwd/default-path executable discovery for logical pathname translation files unless they live under the trusted manifest roots,
  - ban raw relative `load` / autoload / batch execution outside the trusted loader context,
  - normalize and contain all generic relative-load resolution within trusted roots,
  - delete basename-trim and secondary-candidate guessing from generic `load` resolution,
  - require bench/test output to record which trusted Maxima root was used.
- Acceptance:
  - one manifest drives loader, bench, and test tooling,
  - trusted roots are explicit and validated,
  - no host-specific or ambient root guessing remains,
  - logical pathname translation loading cannot execute files from cwd or undeclared defaults,
  - ordinary relative loads either resolve through the trusted loader context or fail explicitly,
  - generic relative load resolution cannot escape trusted roots by `..`, non-canonical truenames, or guessed alternate candidates,
  - benchmark/test output includes trusted-root provenance.
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
- Files: `src/runtime/primitives/package.zig:316-330`, `src/runtime/primitives/package.zig:642-656`, `src/runtime/primitives/package.zig:1711-1716`, `src/runtime/heap.zig:3326-3339`
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

#### 1.3a Make loader special state canonical and dynamic
- Goal: G1, G2, G3
- Files: `src/interp/repl.zig:2038-2066`, `src/interp/repl.zig:2116-2149`, `src/interp/repl.zig:2216-2237`, `lib/stdlib.habu:7319-7347`
- Depends on: 1.1, 1.2, 1.3
- Work:
  - make `load` dynamically and canonically bind `*LOAD-PATHNAME*`, `*LOAD-TRUENAME*`, `*DEFAULT-PATHNAME-DEFAULTS*`, and `*PACKAGE*`,
  - remove alias writes and cwd heuristics used in place of true dynamic special binding,
  - make nested loads and logical-pathname translation loading depend only on this contract,
  - make logical pathname translation loading fail explicitly on real translation-load errors instead of masking them or falling through to unrelated candidates,
  - ensure translation state is derived from the current trusted loader context, not stale preexisting global state.
- Acceptance:
  - nested `load`/autoload/batch flows get truthful dynamic special bindings,
  - path/package context is derived from loader state, not post-hoc alias repair,
  - translation loading has no error-masking cross-candidate fallback after a real load failure.
- Risk:
  - this is load-bearing for nested loads, autoload, and canonical test execution.
- Effort: L

#### 1.4 Make function and macro lookup canonical, package-correct, and generic
- Goal: G1, G2
- Files: `src/interp/repl.zig:1565-1595`, `src/interp/repl.zig:4209-4255`, `../maxima/src/suprv1.lisp:144-175`, `../maxima/src/mlisp.lisp:2037-2117`
- Depends on: 0.3, 0.4, 0.6, 1.2, 1.3
- Work:
  - remove Maxima-specific autoload semantics from generic REPL lookup,
  - implement generic autoload/property lookup without `$`-prefix fallback,
  - make symbol-function/macro-function/special-variable lookup package-correct.
- Acceptance:
  - generic lookup contains no `MAXIMA:AUTOLOAD`, `MAXIMA:LOAD-FUNCTION`, or `$`-prefix special case,
  - upstream Maxima autoload works via generic property/function semantics.
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
  - validate feature-conditionals, dispatch characters, read-time eval, dotted-pair skip semantics, and parser-facing Unicode/string behavior on actual Maxima sources.
- Acceptance:
  - real Maxima source modules named in the manifest parse without local source patches.
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

#### 2.1 Prove interactive and scripted execution on the clean load path
- Goal: G2
- Files: shared manifest/loader entrypoints plus script runner used by direct Habu execution
- Depends on: Phase 1
- Work:
  - run a script that loads Maxima and evaluates representative simplify/factor/solve/integrate forms,
  - prove package/context state survives repeated evaluation.
- Acceptance:
  - interactive and scripted sessions produce stable, repeatable results after clean load.
- Risk:
  - hidden global/package state corruption often appears only on repeated runs.
- Effort: M

#### 2.2 Close condition/restart/unwind semantics used by real Maxima paths
- Goal: G2, G3
- Files: `src/runtime/primitives/condition.zig:65-166`, condition/unwind machinery in `src/interp/vm.zig`, relevant runtime entry points
- Depends on: 0.9
- Work:
  - implement actual handler dispatch,
  - implement restart lookup/invocation,
  - make `break`, `warn`, load-abort, catch/throw, block/return-from, tagbody/go, and debugger-facing flows truthful.
- Acceptance:
  - no advertised restart path is stubbed,
  - Maxima runtime paths depending on restarts/NLX behave correctly.
- Risk:
  - touches core control-flow invariants.
- Effort: XL

#### 2.3 Fix stream and pathname semantics used by batch/test-batch/share runtime
- Goal: G2, G3
- Files: `src/runtime/primitives/io.zig:1687-1725`, stream machinery in `src/runtime/primitives/io.zig`, `src/interp/vm.zig:1834-1843`, `src/interp/vm.zig:5342-5348`, `lib/stdlib.habu` pathname/open helpers, `../maxima/src/mload.lisp:50-73`, `../maxima/src/mload.lisp:172-205`, `../maxima/src/macdes.lisp:28-86`
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
  - use one canonical pathname-designator serialization path across `namestring`, `truename`, directory creation, and filesystem primitives,
  - make primitive `open`/file-create/designator operations consume canonical pathname designators instead of raw cwd-relative strings,
  - bring ordinary file query/read/write helpers under the same canonical/trusted-root contract as `open`,
  - implement CL `open` disposition semantics (`if-exists` / `if-does-not-exist`) correctly and fail closed,
  - bring delete/rename path mutation primitives under the same canonical/trusted-root rules as open/create,
  - bring directory-creation pathname primitives such as `ensure-directories-exist` under the same canonical/trusted-root rules as file opens,
  - close canonical `directory` / wildcard search / `pathname-match-p` semantics under pathname designators, case rules, and trusted-root/no-cwd behavior,
  - derive/protect stream truenames for file-backed streams,
  - propagate correct `$load_pathname` / `*LOAD-PATHNAME*` context through `batch-stream` and `batchload-stream`,
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
- Depends on: 1.1, 2.3
- Work:
  - remove path guessing and swallowed `file_search` failures,
  - refuse to run any rtest unless the shared loader manifest completed cleanly with zero missing/failed modules,
  - validate requested test names,
  - confine resolution to declared testsuite roots,
  - convert break/diff/unexpected-pass outcomes into explicit automation failure.
- Acceptance:
  - tool either resolves via canonical Maxima file search or fails explicitly,
  - no `../` escape or fabricated path fallback remains,
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

#### 3.1 Make `test-batch` the single correctness authority
- Goal: G3
- Files: `../maxima/src/mload.lisp:379-509`, `tools/maxima-rtest.lisp`, any custom runner glue
- Depends on: 2.2, 2.3, 2.4
- Work:
  - remove or demote custom correctness authorities,
  - ensure canonical test-batch path is what automation uses,
  - make the canonical runner fail closed on partial loader state and non-success test outcomes.
- Acceptance:
  - correctness claims cite `test-batch` runs, not ad-hoc loader scripts,
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
  - open focused follow-up items only for concrete gaps.
- Acceptance:
  - no vague “Maxima issue” bucket remains for core tests.
- Risk:
  - requires discipline to keep failures grounded and deduped.
- Effort: XL

#### 3.4 Sweep share tests and share module execution
- Goal: G2, G3
- Files: `../maxima/share/**`
- Depends on: 1.5, 1.7, 2.3, 3.2
- Work:
  - run share tests and representative share packages,
  - classify failures by real Habu feature gap,
  - treat missing load/search/runtime semantics as Habu bugs, not optional misses.
- Acceptance:
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
- Depends on: Phase 2
- Work:
  - benchmark harness must load the same clean environment as scripts/tests,
  - abort on loader failures, missing bindings, patch dependence, or fake FASL/source fallback,
  - require hermetic trusted-root selection,
  - record loader root/module provenance in benchmark output,
  - invalidate the run if any selected workload fails during setup, warmup, or timed execution.
- Acceptance:
  - no timing output is emitted from a dirty or partial Maxima environment,
  - no benchmark result is emitted for a selected set unless every selected workload succeeds,
  - benchmark output records the exact trusted Maxima root and manifest used.
- Risk:
  - current perf numbers may disappear until correctness is real.
- Effort: M

#### 4.1a Make performance and validation tooling hermetic and fail-closed
- Goal: G4
- Files: `tools/maxima-hotspots`, `tools/validate-session`, `tools/perf-loop`, `tools/gc-compare`, `bench/check.zig`, `tools/bench_pack_runner.py`, `tools/comprehensive-bench`, `tools/bench_compare.sh`, `tools/maxima-bench`, `tools/run`, `tools/build`, `tools/build.lisp`, helper scripts they invoke
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
  - require workload-attributed, identity-grounded JIT coverage in support tooling rather than coarse compiled-count proxies,
  - require recommendation output to preserve exact dot provenance or emit no recommended dot,
  - treat benchmark payloads as untrusted data in all helper/report paths; parse from files/stdin/JSON instead of code interpolation,
  - require explicit provenance and policy for any environment-supplied peer-runtime benchmark commands,
  - make malformed/incomplete benchmark payloads fatal before baselines or reports are saved,
  - require strict helper payload schema/version/engine/workload identity validation before any result is treated as authoritative,
  - require authoritative validation helpers to prove payload provenance from the expected repo-pinned producer binary,
  - require build/prepare steps for authoritative helpers to succeed before any benchmark/helper output may be published or saved,
  - require producer commands themselves to exit successfully; parsed payload from a failed child is not authoritative,
  - require generic wrapper tools either to enforce repo-local allowlisted executable policy with provenance capture or to be explicitly demoted from authoritative workflows,
  - require one canonical supported build entrypoint; remove or demote legacy alternate build wrappers outside the cutover/provenance policy,
  - remove or demote legacy comparison/publishing entrypoints that cannot enforce the canonical gate/provenance rules.
- Acceptance:
  - hotspot/validation/perf-loop tooling only consumes hermetic repo-pinned binaries and scripts,
  - no downstream report, history entry, or prioritization output is produced from dirty benchmark runs,
  - no proof gate depends on mutable `/tmp` scripts or cwd-relative binaries,
  - `gc-compare` and `bench/check` cannot emit authoritative clean output from cwd-relative or non-provenance-tracked binaries,
  - `bench_pack_runner.py` top-level status/provenance reflect normalized workload completeness, not raw process success,
  - malformed, partial, or environment-spoofed comparator payloads cannot be saved or published as clean baselines,
  - helper payloads are rejected unless schema, producer identity, and provenance match the expected canonical producers,
  - authoritative helpers cannot run on stale binaries after failed build/prepare steps,
  - authoritative helpers reject parsed payloads from non-zero/signaled producer commands,
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
- Files: `src/jit/candidates.zig:28-31`, `src/jit/candidates.zig:106-159`, `src/jit/candidates.zig:289-307`, `src/jit/candidates.zig:610-630`, `src/testing/compile_chunk.zig:344-408`, `src/testing/compile_chunk.zig:489-509`, `src/interp/repl.zig:3066-3135`, `src/interp/repl.zig:3238-3274`
- Depends on: 4.1
- Work:
  - replace case-insensitive/package-stripping candidate matching with canonical identity-based matching,
  - stop silent skip/no-match behavior in compile helpers,
  - unify helper and runtime hoist admission/compile/status paths so test/bench helpers cannot diverge from product behavior,
  - report eligible/skipped/unsupported coverage explicitly.
- Acceptance:
  - JIT compile/skip metrics are identity-based and auditable,
  - helper/runtime JIT reporting and admission behavior match,
  - unsupported-node distribution is accurate.
- Risk:
  - will reduce reported JIT coverage before it improves.
- Effort: L

#### 4.3 Define the real workload set
- Goal: G4
- Files: `bench/maxima_workload.zig`, benchmark scripts/manifests
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
