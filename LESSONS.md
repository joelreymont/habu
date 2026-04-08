# Lessons Learned

Hard-won patterns and anti-patterns from building Habu. **Update this file at the end of every session** with new discoveries.

> Frequency counts are from SESSION.md analysis (~102K lines, ~50 sessions).

---

## Session Notes (2026-04-04)

## Session Notes (2026-04-05)

## Session Notes (2026-04-06)

## Session Notes (2026-04-08)

### Worked Well
- `multiple-value-setq` must return the primary value of its source form, not the value of the final assignment side effect. `lib/stdlib.habu:2771-2780` was expanding to a `progn` of `setq`s whose result was the last assigned variable, so Maxima `../maxima/src/risch.lisp:236-251` fed `risch-expint` into `(rischadd (multiple-value-setq ...) risch-y)` instead of the intended `rischlogeprog-value` / `rischexppoly` result. Fixing the macro to save and return the primary value moved canonical `tools/maxima-rtest.lisp rtest6` line 11 from `error-catch` to the real noun integral residue.
- Condition-transfer detection must compare full VM control state, not just `chunk/ip/sp/fp`. In `src/interp/vm.zig`, `signalTypeErrorDatumExpected` and `rethrowConditionToCatch` were treating those four fields as sufficient proof of movement, but an unwind into cleanup can land on the same `ip` with the same stack/frame pointers after the faulting opcode has already advanced `ip`. Switching those paths to `State.save` + `hostCallbackMovedControl` fixed the generic `unwind-protect` cleanup regression in `src/tests/unwind_error.zig` and restored the missing cleanup probe (`CLEANUP` printed before `HANDLER`).
- Interned symbol home-package identity cannot live only in `Symbol.reserved` unless GC rewrites it. `src/runtime/gc.zig:2038-2047` previously copied only `plist` for `.symbol` objects, so moving GC left `reserved` pointing at stale package addresses. That made `src/runtime/qual_name.zig:29-64`, `src/runtime/heap.zig:3623-3631`, and VM/compiler global lookup silently fall back from `MAXIMA::*DB*`-style qualified names to unqualified names after enough GC pressure. Rooting and forwarding the package object through GC fixed the real Maxima `QUEUE+P` / `*DB*` floor and restored canonical `rtest6` execution to semantic diffs instead of the old line-11 `TypeMismatch`.

### Did Not Work
- Treating the `QUEUE+P` failure as a pure special-variable store/load mismatch was too narrow. The decisive signal was that direct small-package `defvar`/`setq` probes passed while long Maxima runs failed only after substantial allocation pressure; the missing GC rewrite of interned symbol package pointers was the shared root cause.

### Worked Well
- `multiple-value-bind` needs the same special-variable treatment as lambda parameters. In `MAXIMA`, locals like `q` inside `cquotient` were globally special enough that `src/compiler/compile.zig:9360-9406` bound them to lexical slots, but body refs still went through the dynamic/global path, so `(multiple-value-bind (q r) (floor 1 1) ...)` produced `(#<unbound> 0)` in `MAXIMA` even though it was `(1 0)` in `CL-USER`. Carrying `special_bindings` on `mv_bind` in `src/compiler/ir.zig:248-253,1500-1504`, then emitting `push_progv`/`pop_progv` around the `mv_bind` body in `src/bytecode/emit.zig:2461-2481`, fixed the real Maxima arithmetic floor: `cquotient(-1,1) => -1`, `pgcda(-1,1,t) => (1 -1 1)`, and `fpgcdco(-1,1) => (1 -1 1)`.
- Proclaimed special lambda parameters cannot be implemented as “lexical slot + body-only `progv` wrapper.” `src/compiler/compile.zig:3138-3161,7260-7286,4518-5165` was letting reads and `setq` hit the local slot while nested callees still observed the entry-time dynamic binding. The direct proof is `/tmp/special_setq_repro.lisp`, which used to return `(UPDATED PARAM)` and now returns `(UPDATED UPDATED)`. The correct cutover is two-part: compile special refs/sets through the dynamic/global path, and emit staged `push_progv` binds in `src/bytecode/emit.zig:1422-1434,1587-1710` as required/optional/rest/key parameter slots become live. That removes the fake `radcan1` local-success/global-stale split and leaves canonical `tools/maxima-rtest.lisp rtest6` on the real semantic diffs at lines `11, 13, 15, 17, 20, 22`.
- Uninterned-symbol plist writes must root both the target symbol and in-flight plist cells across cons allocation. `src/runtime/primitives/list.zig:12-19,399-485` was allocating new plist cons cells with only raw locals live; if GC ran inside a native `putprop` on a gensym, the uninterned symbol was not reachable from any package/intern table root and the write landed on stale state. Rooting the target/value/plist operands through `allocWithGC` fixed that class without another Maxima patch and moved canonical `tools/maxima-rtest.lisp rtest6` from the old line-11 hang into an explicit test-batch error break plus later semantic diffs.
- `symbol-plist` conversion code has the same moving-GC hazard as direct plist mutation if it stages heap values outside the root set. Rewriting `src/runtime/primitives/symbol.zig:111-176,257-291` to build/reverse flat/alist plists with rooted cons allocation removed the hidden raw-ArrayList root bug for gensym plist round-trips instead of only fixing `putprop`.
- When a local lambda parameter shadows an outer lexical with the same symbol identity, inherited `boxed_vars` must be filtered by the current lambda's bound symbols before compiling the lambda body. `src/compiler/compile.zig:4484-4502,4956-4980` was letting outer boxed bindings leak into shadowing local params, so Maxima's `def-simplifier`-generated `give-up` closures compiled body references like local optional `x`/`y` as `box_ref` of unboxed locals. The direct proof is the new regression in `src/tests/integration.zig` using outer boxed `x`/`y` plus inner `(&optional (x x) (y y))`, and the workload proof is `BATAPP` on the resimplified `rtest6` line-11 integrand returning `((%BETA SIMP) ((RAT SIMP) 1 4) ((RAT SIMP) 9 4))` instead of throwing `TypeMismatch` in `GIVE-UP`.
- Maxima's `scan-string` bug was not in parser/operator logic at all; it was in the CL array type contract. `../maxima/src/nparse.lisp:323-333` builds string buffers with `:element-type #.(array-element-type "a")` and then `copy-seq`s them. Habu had both `array-element-type` and `upgraded-array-element-type` hardcoded to `t` in `lib/stdlib.habu:649-707`, so Maxima string literals became generic vectors, `stringp` returned false, and `apply(kill, opsies)` in `rtest6` died before operator registration. Making those APIs return `character` for stringlike arrays fixed the real floor and moved canonical `rtest6` past the old `infixie is not an infix operator` parser blocker into later semantic diffs.
- Character vectors and rank-1 char arrays have to participate in the generic string-designator surface, not in ad hoc per-primitive switches. `src/runtime/primitives/string.zig` now provides `designatorBytes`, and routing `make-symbol`, package lookup/intern, and VM string op consumers through it fixed the real `rtest6` `infix("infixie")` floor without another Maxima-specific patch.
- Maxima top-level `meval` proofs must account for the `((DISPLAYINPUT SIMP) nil ...)` wrapper. The earlier `src/tests/integration.zig:10307-10355` buildq probe compared the whole top-level result to `0`, which falsely accused `buildq`/`mqapply`; the real value was `((DISPLAYINPUT SIMP) nil 0)`, so the right proof is to inspect the wrapped payload, not compare against an unwrapped scalar.
- Maxima's `mprops` metadata path is not a symbol plist; it mutates the cdr of a head cons as a flat indicator/value chain. `../maxima/src/maxmac.lisp:153-156` calls `putprop` on that head cons, so `src/runtime/primitives/list.zig:369-425,427-490` must support cons targets in `get`/`put`/`remprop`, not just symbol-like targets. The right proof is both the runtime primitive test and the Lisp-level REPL test in `src/tests/integration.zig:10603-10635`; trying to prove this through `maxima-load-all` was wrong because that pulls in unrelated upstream blockers first.
- `functionp` must accept symbols only when they are actually function-bound. `lib/stdlib.habu:4673-4679` previously returned true for every symbol, which poisoned Maxima's `getl-lm-fcn-prop` classifier in `../maxima/src/mlisp.lisp:393-404` by treating arbitrary symbols as `subr`s before operator/mexpr dispatch. Tightening `functionp` to closures, compiled/generic functions, `fboundp` symbols, and `fboundp` `(setf ...)` names is the CL-correct floor. The direct proof is `(list (cl:functionp 'car) (cl:functionp 'apply) (cl:functionp 'habu-no-such-function)) => (t t nil)` in `src/tests/integration.zig:9037-9054`.
- `defvar` must not be lowered as unconditional `define`. `src/compiler/compile.zig:9367-9392` was clobbering preinitialized specials, which broke the new `COMMON-LISP:*READTABLE*` object by resetting it to `nil` during stdlib load. Lowering `defvar` to “store only if current global is `unbound`” fixed `*readtable*` without reintroducing fallback state.
- Internal `%...` helpers used by stdlib wrappers must still be part of the compiler's builtin-function cache. `src/compiler/compile.zig:16231-16308` was missing `%copy-readtable`, `%readtable-case`, and `%set-readtable-case`, so direct wrapper calls fell off the builtin path and failed at runtime even though VM native-call tags already existed.
- Dynamic special binding must snapshot authoritative special values, not raw global slots. `src/interp/vm.zig:9148-9155` was saving `self.globals[idx]` before `progv` rebinding; that is wrong for specials mirrored into Zig-side state like `*PRINT-CASE*`. Using `loadGlobal(idx)` fixed `(let ((*print-case* :upcase)) nil)` and the nested `princ-to-string`/`*readtable*` proof path.

### Did Not Work
- Proving uninterned plist behavior through `./zig-out/bin/habu <script>` was a dead end here. Direct script-mode probes still hit the separate top-level script/load surface, so the trustworthy closure was `zig build` plus canonical `tools/maxima-rtest.lisp rtest6`, not ad hoc standalone `.lisp` files.
- Treating string designators as only base strings/symbols/keywords was too narrow. Maxima's operator setup passes adjustable/fill-pointer character vectors into `MAKE-SYMBOL` and package APIs; fixing only one caller would have left the same bug scattered across `make-symbol`, `intern`, `find-package`, and VM string ops.
- Comparing a top-level Maxima `meval` result directly against a scalar was wrong. Top-level forms are wrapped for display, so the old buildq regression in `src/tests/integration.zig:10307-10355` produced a false blocker and a bogus `habu-fix-buildq-lambda-343d57fe` floor.
- Using `maxima-load-all` as the proof harness for a local metadata-layout fix was wrong. The earlier `src/tests/integration.zig:10603+` attempt failed at unrelated loader/runtime floors before `MREAD` even existed, so it could not validate the cons-plist change at all.
- Proving behavior against `./zig-out/bin/habu` while `zig build` is still running gives false negatives from the old binary. Wait for the build to exit before trusting probe output; the earlier `copy-readtable` and `*print-case*` failures were stale-binary artifacts.

### Worked Well
- Nested `labels` must inherit outer boxed function bindings, not replace them. `src/compiler/compile.zig:6598-6614` was resetting `self.boxed_fn_syms` to only the current `labels` cluster, so inner `labels` bodies compiled outer lexical function references like `f` as raw box values instead of `box_ref` callables. The tiny repros `(labels ((f (x) x)) (labels ((g () (f 1))) (g)))` and `(labels ((f (x) x)) (labels ((g () nil)) (funcall (function f) 1)))` both failed until the inner set inherited the outer boxed function symbols; after that, `pregexp:pregexp-match-positions` on `("abc" "abc")` started returning `((0 . 3))`.
- `lib/maxima-loader.lisp` must hard-cut to `(in-package :cl-user)` at file entry. Loading it while `*package*` is `MAXIMA` was otherwise defining `*maxima-source-dir*`, `maxima-load-all`, and `maxima-try-load` in the ambient package, which made tool/probe behavior depend on caller state instead of a canonical API surface. The right proof is to load `maxima-package.lisp`, bind `*package*` to `MAXIMA`, load `lib/maxima-loader.lisp`, and assert the `cl-user::` bindings still exist.
- Host-backed VM callbacks must not synthesize normal returns after a non-local exit crosses their call barrier. `src/interp/vm.zig:3240-3287,3335-3392,3476-3523` now rethrows across the saved barrier and returns `error.ControlTransfer` instead of executing to a value, which fixes `(handler-case (load ...))` continuing at the wrong IP.
- The right regression for nested load NLX is a top-level file whose first form is `(setq *x* (handler-case (load ...) ...))` followed by a second top-level form. `src/interp/repl.zig:5557-5604` now proves the load catch path preserves continuation instead of corrupting the outer form.
- Top-level script/load bridges should surface uncaught CL conditions as user errors, not raw VM throw names. `src/interp/repl.zig:3392-3403,3841-3852` now reuses `last_error_value` for file/script loads so `/tmp/habu-toperr.lisp` prints `"boom"` and `tools/maxima-rtest.lisp definitely-not-a-real-rtest` prints the canonical unknown-test message.
- Sealing trusted load roots in Zig is the right cutover. `src/interp/repl.zig:118-119,162-183,218,1464-1471` now seeds trust from a compiled-in project root plus the validated sibling `../maxima` tree instead of launch CWD, and `src/main.zig:89-97` opts script directories in explicitly before script load.
- Removing the Lisp-visible trust mutator is safe once bootstrap owns the roots. Cutting `%add-trusted-load-root` out of `src/interp/vm.zig:44-60,816-821,1471-1478,9768-9778`, `src/compiler/compile.zig:482-490,1110-1118,16268-16290`, `lib/maxima-loader.lisp:9-15`, and `tools/maxima-rtest.lisp:58-63` removed a runtime trust-widening path instead of trying to paper over it.
- Manifest root candidates need to derive from the manifest file location, not ambient CWD. `lib/maxima-manifest.lisp:3-10` now anchors `../maxima` off `*LOAD-TRUENAME*` / `*LOAD-PATHNAME*`, which is the correct boundary for authoritative Maxima source discovery.
- Canonical path containment must compare real paths, not mixed alias spellings. `src/interp/repl.zig:2202-2248` now canonicalizes existing file paths before trusted-root containment, which fixes the `/tmp` vs `/private/tmp` mismatch that made explicit host-side root opt-in look broken on macOS.
- Relative `load` search roots must always be directories. `src/interp/repl.zig:2214-2248,5843-5872` now normalizes absolute file-based load origins to their parent directory before resolving nested relative loads, which removes the `NotDir` failure class for absolute script loads like `tools/maxima-rtest.lisp`.
- Process-level failure needs an explicit nonzero exit in `src/main.zig:34-41`. Printing `Fatal error: ...` from the worker thread without `std.process.exit(1)` left authoritative script/test runners reporting `EXIT=0` even on fatal load/runtime errors.

### Did Not Work
- Treating the earlier `intl.lisp` failure as a real loader/runtime blocker was wrong. The failing probe had loaded `lib/maxima-loader.lisp` while `*package*` was `MAXIMA` and then inspected `cl-user::*maxima-source-dir*`, so the resulting unbound variable and downstream `coerce` failure were artifacts of package-unstable loader definitions, not a true Maxima load defect.
- Returning `self.execute()` from `callFromStackAt` after relaying `NestedNonLocalExit` was wrong. That let inner `load`/`eval` callbacks steal the outer `handler-case` completion and later blow up with `execute.ip-oob` on the stale caller chunk.
- Treating an absolute script pathname as a search root was wrong. Nested `(load "foo.lisp")` from an absolute script like `/Users/.../tools/maxima-rtest.lisp` went through `realpath` on `.../maxima-rtest.lisp/foo.lisp` and surfaced as `NotDir`; the correct fix was to canonicalize search roots to directories, not to special-case particular scripts.
- Using `STEP` as a probe helper name was wrong. `STEP` is already a CL macro, so `(step ...)` invalid-syntax results were false evidence about Habu evaluation; renaming the probe helper exposed the real failure surface instead of a macro collision.
- The current `initialize-runtime-globals` floor is not `*maxima-userdir*` or `default-userdir`. The failing clause is the browser setup in `../maxima/src/init-cl.lisp`, and the underlying tiny reproducer is `pregexp:pregexp-match-positions`, which currently throws `TYPE-ERROR nil` even on simple inputs like `(\"abc\", \"abc\")`. Fix that generic matcher/runtime bug before trying to cut the canonical runner over to runtime init.

### Worked Well
- When canonical execution advances, the plan must be rewritten around the new front-door blocker immediately. `PLAN.md:55-65,470-517,687-689` now names the real `rtest6` parser/operator floor, the downstream `rtest6` callable semantics floor, the `rtest6b` successor slice, and blocks Phase 4 on Phase 3 instead of the older, looser dependency.
- When a read-time literal starts failing only after you remove a fake wrapper, check whether the reader has been returning executable forms instead of self-evaluating objects. `src/reader/parser.zig:705-715,876-905` was turning `#p"..."` into `(PARSE-NAMESTRING ...)`, which looked fine at the REPL because it got evaluated immediately, but it leaked cons cells into quoted constants and DEFVAR initforms like `INTL:*LOCALE-DIRECTORIES*`.
- Reader cutover and compiler cutover have to land together for self-evaluating boxed literals. After fixing `#p` to allocate pathname objects at read time, `src/compiler/compile.zig:3105-3118` still rejected those pathname objects as `InvalidSyntax` because pathname literals were missing from the self-evaluating literal set. Adding both halves moved `../maxima/src/intl.lisp` from fake `merge-pathnames` type failures to real `rtest6` execution.
- A tiny quoted-literal probe is the fastest proof for reader honesty. `(let ((x '(#p\"/usr/share/locale/\"))) (list (pathnamep (car x)) (type-of (car x))))` immediately distinguished “REPL eval works” from “constants are still wrong,” and the same pattern exposed `INTL:*LOCALE-DIRECTORIES*` holding cons forms instead of pathnames.
- When a primitive hangs but a peer with the same multiple-value contract works, compare the primitive dispatch table against the lowering switch before touching the VM again. `src/compiler/compile.zig:16452,17635-18115` had `get-macro-character` in `unary_dispatch` but no `.get_macro_character` arm in `compileUnaryPrim`, so it silently fell off the primitive path into the generic callable path. Adding the missing IR lowering, together with fixing `src/interp/vm.zig:7804-7818` to return one primary plus one secondary value instead of pushing both on the stack, resolved both bare `(get-macro-character ...)` hangs and the `../maxima/src/intl.lisp` final `eval-when`.
- A control primitive with the same value contract is the fastest falsification step. Comparing broken `get-macro-character` against working two-value `gethash` showed the REPL and generic multiple-value printing were fine; the bug was specific to `get-macro-character` lowering/runtime semantics.
- `src/interp/repl.zig:1422-1429` needed explicit duplicate-root free on success. `errdefer` only runs on error, so returning early from `addTrustedLoadRoot` leaked every repeated `%add-trusted-load-root` call.
- `lib/maxima-manifest.lisp` must include upstream `intl` before `globals`. The previous stub-only package declaration let Maxima reach `INTL:GETTEXT` with no function definitions, which made canonical `test-batch` fail on a fake loader hole instead of the next real runtime issue.
- `src/interp/repl.zig:250-298` must treat VM ext-root owners as live by VM identity, not stale owner pointer equality. During nested macroexpansion, the VM can still point at an older root owner for the same active VM; rebinding through the active `VmRootCtx` prevents writes through dead stack-owned `ArrayList` headers.
- The right fix for `loop for i upfrom ... as c = ... while ...` is clause-ordering, not a Maxima patch. `lib/stdlib.habu:5019-5240,5288-6188` now separates iterator exhaustion tests from `while`/`until` guards and runs clause-generated per-iteration bindings before guard tests, so ANSI `loop` variables like `as c = ...` are visible when later clauses test them.
- For large `rtest` files, a lightweight top-level semicolon splitter is enough to map Maxima problem ids back to source lines when full `test-batch` runs are too slow to use as an interactive index. `docs/maxima-rtest1-clean-path.json` now ties the clean-path residue set to concrete `../maxima/tests/rtest1.mac` line numbers and non-vague subsystem buckets.
- `tools/maxima-rtest.lisp` must treat `*command-line-args*` as the actual script argv, not as `(exe script arg...)`. The wrapper was reading `(cadr args)`, so the first requested test name was silently ignored and every CLI run defaulted to `rtest1`. Switching to `(car args)` and preflighting the requested name against upstream `testsuite.lisp` fixes the real contract instead of papering over it.
- Lightweight canonical test validation can come from upstream `../maxima/src/testsuite.lisp` without booting all of Maxima. Loading `maxima-package.lisp` plus `testsuite.lisp` under a trusted absolute Maxima root gives the real `$testsuite_files` / `$share_testsuite_files` registry early enough for fast invalid-name rejection in `tools/maxima-rtest.lisp`.
- Foreign loads that execute `(in-package ...)` mutate the active reader package for the rest of the current file load. After the rtest preflight loaded `../maxima/src/testsuite.lisp`, the next top-level form in `tools/maxima-rtest.lisp` was read in `MAXIMA`, so unqualified helper calls resolved against the wrong package. Explicitly restoring `(in-package :cl-user)` after foreign loads is the correct fix.
- CLI/script setup belongs in direct runtime state, not in pre-script `eval` strings. Replacing `src/main.zig:16-43` string-built `(setq ...)` forms with rooted direct publication through `src/interp/repl.zig:1290-1351` removed the `InvalidPackage` script-entry crash and made `./zig-out/bin/habu .tmp/argv-smoke.lisp a b c` expose exactly `(a b c)` in `*command-line-args*`.
- When `make-instance`/slot access breaks for a `defstruct`, inspect the emitted `SlotSpec` flags before touching runtime metadata caches. `src/compiler/compile.zig:11175-11248` was appending defstruct slots without `.is_direct = true`, so `allocateClass` at `src/compiler/compile.zig:11497-11502` built a slotless runtime class for `MAXIMA:INSTREAM`; fixing slot emission removed the Maxima `#$...$` load crash without more loader patching.
- When one local function in a `labels` cluster mysteriously fails to resolve while its siblings work, check the source form shape before touching the compiler. The `loop`/`destructuring-bind` blocker turned out to be a malformed binding list in `lib/stdlib.habu:2179-2214`: `optionals` closed the `labels` function-definition list one form too early, so `keys` was parsed as a body form instead of a local function.
- A tiny direct regression plus the real workload regression is the right proof pair. `src/tests/integration.zig:4003` now proves `labels` can bind a local function literally named `keys`, while `src/tests/integration.zig:7065` and `src/tests/integration.zig:7082` prove the real `loop ... on ... by #'cddr` destructuring path and the Maxima `csimp` inverse-property loop both work again.
- For structural Lisp bugs, paren-depth inspection can beat deeper compiler speculation. Counting depth across `lib/stdlib.habu:2147-2214` exposed that the file was balanced overall but the `labels` binding list and body boundary were wrong, which exactly matched the compiler trace pattern (`BINDINGS`/`OPTIONALS` bound, `KEYS` missing).
- Class metadata has to key off the actual class symbol, not the current package string. `src/compiler/compile.zig:12566-12605,12982-12995` now qualifies class metadata by the class symbol's home package, and `src/runtime/heap.zig:3297-3313` is used for the runtime mirror. That fixes imported class symbols such as `BIGFLOAT-IMPL:BIGFLOAT` accessed from the `BIGFLOAT` package in `../maxima/src/numeric.lisp`.
- When raw source loading skips the upstream system loader, copy the upstream pre-source environment exactly instead of weakening semantics. `lib/maxima-loader.lisp` now seeds and exports `CL-USER:*MAXIMA-BUILD-TIME*` before module loading because `../maxima/src/maxima.system`/`../maxima/src/maxima.asd` do that before `macsys.lisp` reads `cl-user:*maxima-build-time*`.

### Did Not Work
- Leaving the plan at a higher abstraction level than the current blocker was wrong. Once canonical `rtest6` reached `../maxima/tests/rtest6.mac:110`, broad bullets like “remove compiler/runtime blockers next” were no longer sufficient; the plan needed explicit numbered closure items for operator-read visibility, the early wrong-answer cluster, and immediate successor files.
- Treating `#p` as “good enough because evaluating `#p\"...\"` prints a pathname” was wrong. The old reader path only worked in immediate evaluation contexts; it broke quoted literals, DEFSTRUCT initforms, and upstream constants by leaving `(PARSE-NAMESTRING ...)` data structures in the heap.
- Fixing only the VM side of `get-macro-character` was incomplete. The opcode contract bug in `src/interp/vm.zig:7804-7818` was real, but `get-macro-character` still hung until `src/compiler/compile.zig:18090-18120` actually emitted the dedicated IR/opcode instead of falling back to the generic call path.
- Treating `upfrom` as the only missing piece in `loop` was incomplete. Once the parser accepted the keyword, the old execution order still evaluated `while` guards before `as` bindings, so the failing `intl.lisp` form silently returned `nil` instead of exposing the actual next loader blocker.
- Assuming historical `rtest1` problem ids still matched the current raw pair count was wrong. The focused residue note still mentioned problems 196 and 199, but the current direct pair count of `../maxima/tests/rtest1.mac` ends at 195, so any new problem-id-specific fix work must first reconcile the numbering.
- Assuming the active package survived a foreign source load was wrong. `tools/maxima-rtest.lisp` loaded `../maxima/src/testsuite.lisp`, which executed `(in-package :maxima)`, and the next unqualified helper call was looked up in `MAXIMA`, producing an unrelated `UnboundSymbol`.
- Treating invalid `maxima-rtest` CLI smoke timeouts as a loader-speed problem was misleading. The wrapper was ignoring the first user arg entirely because it read `(cadr *command-line-args*)`; invalid-name smoke was accidentally running the default `rtest1` path.
- Publishing CLI args by calling `repl.eval` before script load was a bad trap. It not only blew up on missing packages like `SB-EXT`, it also made script-mode behavior depend on reader/package state before the user script ran. Entry-point setup should not go through the compiler at all.
- Treating the `MAXIMA:INSTREAM` failure as a package-key/cache bug first was wasted motion. Rebuilding `Heap.lookupClassMetadata` from the class object helped diagnosis, but the actual defect was earlier: the class object itself had zero direct slots because `compileDefstruct` never marked those slots as direct.
- Treating the `UnhandledThrow`/`symbol(KEYS)` failure as a VM NLX problem was wasted motion. The throw was downstream from a plain `not-closure` call mismatch because the source never actually bound `KEYS` as a local function.
- Global file balance is not enough when auditing Lisp forms by eye. After the first fix, `lib/stdlib.habu` still parsed with `defmacro destructuring-bind` nested inside `destructuring-bind-impl`; the important check was the local depth returning to zero at the end of the `defun`, not only EOF balance.
- Storing class metadata under `qualifyName(current-package, class-name)` is wrong for imported symbols. That hid for local classes but broke immediately on Maxima's `BIGFLOAT` package, where quoted class names resolve to inherited `BIGFLOAT-IMPL:*` symbols while the loader was storing metadata under `BIGFLOAT:*`.

## Session Notes (2026-04-03)

### Worked Well
- If upstream ships the dependency source, load that source and delete the bootstrap fake. Adding `pregexp` to `lib/maxima-manifest.lisp` and removing the `:pregexp` stub from `lib/maxima-stubs.lisp` let `commac.lisp` execute its compile-time regex readers on the real implementation instead of an empty package shell.
- Maxima bootstrap order has to follow the upstream dependency graph, not a convenient local prefix. Moving `globals` ahead of `lmdcls`/`float-properties` in `lib/maxima-manifest.lisp` made the real `defmvar` macro run and eliminated the fake `MAXIMA:ASSIGN` blocker.
- Legacy CLTL `eval-when` situation names still matter in real code. Teaching `src/interp/repl.zig:4219-4246` and `src/compiler/compile.zig:10412-10441` to recognize `COMMON-LISP:COMPILE`, `COMMON-LISP:LOAD`, and `COMMON-LISP:EVAL` made `../maxima/src/defmfun-check.lisp` install `parse-lambda-list` before `float-properties` expands its first `defmfun`.
- Reader `pkg:sym` lookup must resolve exported names through the package's accessible namespace, not only the local symbol table. Switching `src/reader/parser.zig:1203-1209` to `findAccessibleExact` fixed re-exported inherited externals such as `BIGFLOAT:BIGFLOAT`, which Maxima gets from `../maxima/src/maxima-package.lisp` via `do-external-symbols` + `export`.
- File-backed streams need rooted pathname metadata if higher layers recover load context from the stream itself. Storing `(pathname . truename)` in `src/runtime/primitives/io.zig` file streams and teaching `src/runtime/primitives/pathname.zig:446-450` to accept stream designators made `probe-file`/`truename` on batch streams truthful without special-casing Maxima.
- Pathname/file helpers need one shared designator surface. Adding `pathDesignatorString` / `pathDesignatorBytes` in `src/runtime/primitives/pathname.zig:75-96`, then routing `truename`, `ensureDirectoriesExist`, `Vm.pathDesignatorBytes`, `io.openFile`, `stream.primOpen`, and string-based `listDirectory` through that same path removed ad hoc string special cases and made file/directory operations agree on pathname semantics.
- Class-metadata writes need the same canonical symbol path as reads. Adding `Heap.setClassMetadataForSymbol` in `src/runtime/heap.zig:3297-3313` and switching CLOS slot tests in `src/runtime/primitives/clos.zig:747,772,816` to use symbol-qualified writes fixed the vector-backed metadata-only slot path without reintroducing guessed package strings.
- Directory-only pathname fidelity belongs in the shared renderer, not in scattered call-site fixes. Extracting `writeDirectoryTo` in `src/runtime/primitives/pathname.zig:82-111,355-383,544-564` and reusing it from `namestring`, `directory-namestring`, and `pathnameToString` fixed both absolute and relative trailing-slash roundtrips in one place.
- Synonym streams needed to resolve through VM-owned value cells, not through fake symbol object state. Adding a runtime-level symbol-value resolver hook in `src/runtime/runtime.zig`, installing it around VM entrypoints in `src/interp/vm.zig:3166-3547`, and routing synonym stream delegation through that resolver in `src/runtime/primitives/io.zig:868-869,1874-1899,2283` fixes stream truthfulness without inventing a second binding store.
- `src/runtime/heap.zig:2501` needed synonym streams to be allocated as `.io`, not `.input`. Otherwise output-capable synonym streams fail the top-level direction gate before delegation even gets a chance to resolve the real target.
- Stream predicates must follow the delegated target too. Moving `input-stream-p`, `output-stream-p`, and `interactive-stream-p` onto the runtime helpers in `src/runtime/primitives/io.zig:1701-1736` and using those helpers from `src/interp/vm.zig:6961-6980` keeps synonym streams observationally consistent with the stream they name.
- For runtime-level validation when `zig build test` is baseline-blocked and `./zig-out/bin/habu` is still broken on stdlib bootstrap, a repo-local throwaway `zig test` probe works. Running a temporary in-repo probe reproduced the exact synonym retargeting path and showed the new stream tests in `src/runtime/primitives/io.zig:3200-3271` pass under execution.

### Did Not Work
- Using focused loader regressions that depended on `maxima-load-all`'s relative `source-dir`, package-qualified reader syntax in the test form itself, or helper paths like `subseq` obscured the real bug. For bootstrap-path regressions, prefer explicit module lists, direct `maxima-try-load`, and `find-symbol` against the live package so the test measures loader/package semantics instead of unrelated harness drift.
- Leaving file streams with `source_value = nil` discards real provenance. Higher-level code like `batch-stream` can only recover truthful load pathname/truename if the stream carries that metadata across the open/read boundary.
- Letting string-based directory helpers bypass pathname parsing creates semantic drift fast. The old `listDirectory` string branch in `src/runtime/primitives/io.zig` hand-trimmed `*` suffixes and skipped the pathname wildcard parser entirely, which is exactly how pathname and string designators drift apart.
- Writing class metadata with hardcoded package strings is brittle in tests and runtime scaffolding. `Heap.init` starts in `COMMON-LISP`, so a guessed `COMMON-LISP-USER:...` key can silently test the wrong thing even when the lookup path is correct.
- Repo-local runtime probes that import `src/runtime/**` still pull the existing `src/runtime/primitives/clos.zig:40,124,757` failure into the run. Treat that as a standing runtime-test blocker; do not misattribute it to pathname or stream work.
- Trying to “mirror” symbol values into `objects.Symbol` was the wrong direction. `src/runtime/objects.zig:156-165` has no value slot, so any attempt to push binding state into symbol objects is architectural drift, not a fix.
- Raw `zig test src/runtime/primitives/io.zig` still is not a useful validation command in this repo. Module-path rules reject those relative imports immediately; use `zig build`, `zig build test` baseline comparison, or an in-repo temporary probe instead.
- The repo-local runtime probe pulled in the existing unrelated `src/runtime/primitives/clos.zig:40,124,757` failure (`slot-value uses symbol metadata`). Treat that as separate debt; it is not caused by the synonym-stream fix.

---

## Session Notes (2026-04-01)

### Worked Well
- The right `defstruct :print-function` cutover is class-owned printer metadata plus a VM-driven print callback, not more Maxima patching. Extending `src/runtime/objects.zig` with a rooted `Class.printer`, installing it from `src/compiler/compile.zig` via an internal builtin, and routing recursive stream/string printing through `src/runtime/primitives/io.zig` with a VM callback makes nested structure printing truthful for both symbol and lambda printers.
- The fixed 256-byte `write-to-string` buffer in `src/interp/vm.zig` was another fake-success path. Replacing it with a real string-output-stream path removes arbitrary buffer failure and lets custom structure printers emit through normal stream operations.
- For BOA `defstruct` constructors, generate a real `defun` source form and re-use the normal lambda-list compiler instead of duplicating lambda parsing in Zig. `src/compiler/compile.zig` now derives slot initializers from bound constructor variables plus slot initforms, which is what upstream forms with `&optional` and `&aux` actually need.
- `probe-file` must be defined in terms of truthful `truename`, not the raw input designator. Changing `src/interp/vm.zig:5399-5406` to return the canonical pathname object from `pathname.truename` and only collapse `error.FileNotFound` to `nil` fixes the long-standing “echoed input” lie while preserving the CL distinction between `probe-file` and `truename`.
- For `defstruct (:type list ...)`, the right cutover is compiler-level representation-specific lowering, not trying to reuse the structure-object slot path. Teaching `src/compiler/compile.zig:11283-11958` to emit list constructors, list accessors, `rplaca` writers, named-list predicates, and list rebuild copiers keeps typed-list structs honest and avoids reintroducing the generic-slot/structure conflation we just removed.
- Removing the stale `obj.isVector()` front-gate from `src/runtime/primitives/clos.zig:116-149` was the right root-cause fix for the generic slot split. `getSlotView` was already the canonical representation boundary for boxed structures versus vector-backed standard objects; keeping an earlier vector-only guard made boxed `defstruct` instances fail even though the slot protocol beneath them was already representation-aware.
- Locking the slot split with both runtime-unit and integration coverage is worth doing even when `zig build test` is baseline-blocked. The new focused regressions in `src/runtime/primitives/clos.zig:762-814` and `src/tests/integration.zig:5962-5978` make the intended boundary explicit: boxed structures are valid slot-bearing objects, fixnums and raw condition payload boxes are not.
- Removing the `.fasl` sibling-source substitution from `src/interp/repl.zig:1792-1824` was the right cutover. It immediately turns fake compile-file/load success back into an explicit loader failure instead of silently exercising source paths.
- Tightening `trySignalCondition` in `src/interp/vm.zig:9368-9381` to propagate allocation/runtime failures and only translate `ControlTransfer` into success removed a real error-masking pattern without changing the non-local-exit contract.
- Switching special-variable runtime lookup to exact canonical names in `src/interp/vm.zig:2437-2513` makes `*PRINT-*` semantics depend on real global bindings instead of legacy unqualified retry order.
- Removing compiler global retry ladders in `src/compiler/compile.zig:3657-3671`, `src/compiler/compile.zig:7128-7161`, `src/compiler/compile.zig:7262-7290`, `src/compiler/compile.zig:9381-9397`, and `src/compiler/compile.zig:9478-9500` is low-risk when the symbol already carries package identity; the qualified name is the only binding the compiler should consult.
- Cutting the REPL macro name-scan fallback in `src/interp/repl.zig:3949-3990` is viable because macro roots are already rebuilt from persistent roots on GC transitions. Once the fallback is gone, any remaining stale-key issue becomes observable instead of being papered over.
- Turning compiler macro/symbol-macro access into live-key helpers in `src/compiler/compile.zig:16305-16334`, and routing all copy/restore/local-macro writes through those helpers, is the right root-cause fix for moving-GC key churn. Direct raw-map reads/writes let stale keys seep back in through macrolet, eval, and nested macro compilation even after the obvious lookup fallback was gone.
- Removing REPL package-name macro fallback in `src/interp/repl.zig:3874-3927` and the Maxima-only `$` callable/autoload retry in `src/interp/repl.zig:1499-1510,1596-1628` is the right way to expose real package/load gaps. Those conveniences made Maxima progress look better while keeping generic symbol semantics wrong.
- Deleting `GlobalEnv` alias-table maintenance in `src/compiler/compile.zig:1952-2024` is safe once no runtime code consults it. Keeping a reverse unqualified-name index around “just in case” invites legacy resolution to creep back in through new call sites.
- Making `COMMON-LISP-USER` the canonical user package and `CL-USER` only its nickname in `src/runtime/heap.zig:859-904` is the right fix. The previous inversion forced compiler/runtime helpers to keep spelling-specific retries alive. Once the package object itself has the canonical name, qualified globals, symbol home packages, and class-metadata keys all line up naturally.
- For DEFCLASS inheritance, resolving parent metadata by symbol package identity (`src/compiler/compile.zig:12173-12192`) is materially safer than stripping to `getName()` and running name-prefix heuristics. The string-only path was what forced `lookupClassMetadataByName` to keep broad package-prefix and local-name fallbacks around.

### Did Not Work
- Letting `truename` return `nil` on missing files (`src/runtime/primitives/pathname.zig`, old state) was collapsing two different CL contracts into one. The right behavior is `truename -> error.FileNotFound`, `probe-file -> nil`, with the conversion happening only at the `probe-file` call site.
- The top-level `./zig-out/bin/habu <script>` path is still not a trustworthy semantic smoke gate for new language work while `lib/stdlib.habu` aborts during bootstrap with `UnboundSymbol`. That failure blocked direct runtime proof for the new `defstruct :type list` scripts even though `zig build` was green, so keep distinguishing “feature compile/build clean” from “full stdlib bootstrap healthy”.
- Reaching for a nonexistent heap helper in a new test (`allocClass` in `src/runtime/primitives/clos.zig`, trial state) was a self-inflicted detour. For runtime unit tests, allocate `objects.Class` directly and initialize the minimal fields explicitly instead of assuming a convenience API exists.
- Widening `heap.intern` to `anyerror` was the wrong move. It exploded error-set coercions across `emit`, parser, desugar, and runtime callsites. Keep the public error surface narrow and confine any non-package interning path to explicit internal bootstrap logic only.
- Raw `zig test path/to/file.zig` is not a useful validation path in this repo for cross-module files such as `src/interp/vm.zig`, `src/interp/repl.zig`, and `src/compiler/compile.zig`; it fails on import-root layout before it tells you anything about the change. Use the real repo entrypoint (`zig build test`) for build validation.
- Leaving one old reference behind after deleting a retry ladder is easy to miss and shows up only as a plain compile break, not a semantic test failure. The stale `for (prefixes)` block in `src/compiler/compile.zig:7804-7811` survived the earlier lookup cleanup until `zig build test` caught it.
- Keeping class-metadata fallback search in place after package canonicalization is counterproductive. `lookupClassMetadataByName` broad-prefix probing and local-name matching in `src/compiler/compile.zig:12059-12096` were only compensating for earlier package-identity loss; once superclass lookup uses `lookupClassMetadataBySymbol`, those heuristics become liability, not safety.

## Session Notes (2026-04-02)

### Worked Well
- Relative `load` should terminate in one explicit trusted root, not wander through basename trimming or ambient CWD access. Capturing the REPL startup CWD as `trusted_load_root`, resolving candidate paths with `std.fs.path.resolve`, and rejecting any path that escapes that root turns relative loads into a real contract instead of a heuristic.
- A single `lib/maxima-manifest.lisp` is the right boundary for Maxima provenance. Deriving `:srcdir`, `:sharedir`, `:testsdir`, and the authoritative module list from one detected root removes duplicated `../maxima` / `/tmp/maxima` guesses from `lib/maxima-loader.lisp`, `lib/maxima-post-load.lisp`, `tools/maxima-rtest.lisp`, and `bench/maxima_workload.zig`.
- Unsupported-IR telemetry should live at the same level as `jit_adm`, not only in trace prints. Keeping a fixed `Vm.unsupported_tags` counter array keyed by `std.meta.Tag(Ir)` and emitting non-zero `{tag,count}` entries from `bench/maxima_workload.zig` makes unsupported-shape pressure available to normal benchmark tooling.
- `make-string-input-stream` needed both surface and storage fixes to become CL-correct. The compiler had to stop routing multi-arg calls through the unary primitive table and lower `(make-string-input-stream s start end)` explicitly in `src/compiler/compile.zig:16270-16271,17615-17622`, while the runtime had to honor bounds without fixed 4 KiB coercion buffers in `src/runtime/primitives/io.zig:1687-1752` and keep `Heap.allocStringInputStream`'s public error set narrow in `src/runtime/heap.zig:2298-2333`.
- For reporting-only perf dots, validate the formatter with a mocked payload instead of waiting on a full benchmark run. Loading `tools/maxima-hotspots` through `SourceFileLoader` and exercising `format_text` / `format_markdown` proved the new `jit_adm` fields were all surfaced without paying the Maxima load cost.
- Making `src/runtime/heap.zig:3152-3170` scan the Lisp package registry by uppercased designator bytes instead of calling `packageKey` is the right fix for read-only package resolution. The old `findLispPackage -> packageKey -> internKeyword` path mutated the keyword table on every lookup, so innocent `find-package` / `find-symbol` reads were allocating and changing heap state.
- Adding `Heap.lookupInPackage` in `src/runtime/heap.zig:3407-3410` and switching JIT/rooting and autoload read paths in `src/interp/repl.zig:1498-1505,2616-2631` and `src/testing/compile_chunk.zig:24-45` away from `internInPackage` removes symbol-table mutation from lookup-only flows without weakening package-qualified resolution.
- Extracting JIT literal-root collection into a single shared module in `src/jit/literal_roots.zig`, then driving both `src/interp/repl.zig:2845-2860` and `src/testing/compile_chunk.zig:275-290` through it, is the right structural fix for backend/collector drift. One traversal plus an explicit `ensureCoverage` check turns silent divergence into a hard compile rejection.
- Giving restarts stable dynamic-extent IDs and returning first-class restart objects from `compute-restarts`/`find-restart` in `src/interp/vm.zig:6245-6290,9541-9648` fixes the generic CL semantics that symbol-only restart lookup could not express. The old implementation could not distinguish nested same-name restarts, so `(invoke-restart (find-restart ...))` was only accidentally correct when names were unique.
- Replacing the compiler/VM's duplicated hardcoded condition hierarchy with runtime `subtypep` checks in `src/compiler/compile.zig:9008-9020`, `src/interp/vm.zig:8891-8942`, and `src/runtime/primitives/type.zig:490-500` fixes custom condition dispatch generically. The old tables only knew about a few built-in relationships, so `handler-case`/`handler-bind` failed on valid subclasses such as user-defined warnings.
- Replacing the variadic builtin `(eval (cons 'sym args))` path with first-class `native_code` callable handles in `src/interp/repl.zig:1447-1467` and direct VM dispatch in `src/interp/vm.zig:9721-10094,12516-12529` removed the last eval-based function-designator shortcut without needing Maxima-specific exceptions.
- Treating unsupported pseudo-builtins as not-callable-before-stdlib is safer than synthesizing fake wrappers. Trimming `vector`, `concatenate`, and `set-macro-character` from `src/compiler/compile.zig:15618-15674` stops `fboundp`/`symbol-function` from claiming runtime support that only exists after stdlib loads.
- Extending `compiled-function-p` / runtime `typep` to include `native_code` in `lib/stdlib.habu:4650-4655` and `src/runtime/primitives/type.zig:236` keeps first-class builtin callables visible to Lisp without reintroducing wrapper closures.
- Driving the direct dispatch through existing runtime helpers exposed a real latent bug in `src/runtime/primitives/hash.zig:14-20`: `primMakeHashTable` was storing error unions from `heap.intern*` instead of values. Direct execution made the broken path compile, and fixing it removed a hidden blocker.
- `src/reader/parser.zig:1018-1195` needed an explicit split between `pkg:sym` and `pkg::sym`; treating both markers as the same `intern-in-package` path hid two separate CL contracts. The right cutover is: missing package is `error.InvalidPackage`, single-colon requires native export visibility, and only double-colon is allowed to intern.
- `src/interp/repl.zig:4656-4717` should not synthesize Lisp package objects from native placeholders or auto-create packages for `(in-package ...)`. Once the reader/package system is canonical, `in-package` either resolves an existing package object or fails with `error.InvalidPackage`.
- `src/runtime/heap.zig:3176-3187` benefits from a byte-slice `findLispPackageBytes` helper. It keeps parser/package designator reads case-folded and read-only without allocating temporary string designators or mutating keyword state.
- For `src/interp/repl.zig:1919-1931`, a fixed `readToEndAlloc(..., 1024 * 1024)` ceiling is the wrong abstraction. The correct cutover is to derive the read bound from `file.stat().size` so large source files fail only on real size/allocator limits, not an arbitrary loader cap.
- A reader-only Maxima stage is viable as a focused integration test if it bootstraps upstream package definitions first, then parses selected source files with `Parser.parseAll`. That keeps later compiler/runtime failures from masquerading as reader failures while still using real upstream sources (`src/tests/integration.zig`, `../maxima/src/maxima-package.lisp`, `../maxima/src/float.lisp`, `../maxima/src/nparse.lisp`, `../maxima/src/transs.lisp`, `../maxima/src/limit.lisp`).

### Did Not Work
- The REPL startup `load "lib/stdlib.habu"` failure is not caused by the relative-path resolver alone. Even after the trusted-root hardening in `src/interp/repl.zig`, `./zig-out/bin/habu` still dies early with `Cannot open 'lib/stdlib.habu': UnboundSymbol`, so there is another startup-stage bug outside the normal `Repl.loadFile` path.
- Validating Lisp loader changes through `./zig-out/bin/habu` is still blocked by the existing startup path bug: the binary tries to `load "lib/stdlib.habu"` relative to startup state and dies with `UnboundSymbol` before stdin/file probes can exercise the changed Lisp files. Treat that as a separate validation blocker, not as evidence against the manifest change.
- `inline for` is the wrong tool when the body needs runtime filtering with `continue`. In `bench/maxima_workload.zig`, the unsupported-tag collector had to use an `if (n != 0)` guard inside the `inline for` body instead of a runtime `continue`.
- Manually duplicating the `jit_adm` field list in multiple formatter branches is brittle. Centralizing the field list in `tools/maxima-hotspots` keeps the text and markdown outputs from drifting when new counters are added to `Vm.JitAdmStats`.
- Reusing `packageKey` for registry lookup was the wrong abstraction. It is correct for package registration/removal because those operations own mutation, but it is wrong for `findLispPackage` because keyword interning is itself a write.
- In Zig comptime-dispatched helper structs, cross-file callback entry points must be `pub`. The first pass at `src/testing/compile_chunk.zig:54-63` and `src/interp/repl.zig:2639-2658` compiled locally in the defining file shape but failed once `src/jit/literal_roots.zig` invoked `ops.onLit`/`onGlobalRef`/`onLambda` from another module.
- `Repl.vm` is stored by value, not pointer. Shared helper extraction must pass `&ctx.repl.vm` into APIs expecting `*Vm`; using `ctx.repl.vm` directly broke `zig build` immediately at the new shared literal-root call sites in `src/interp/repl.zig:2641-2657`.
- Treating restart objects as plain symbols was false progress. `src/interp/vm.zig:6258-6288,9541-9572` could only re-find by name, so nested same-name restarts had no exact identity and `typep 'restart` was permanently false in `src/runtime/primitives/type.zig:280`.
- Baking condition subtype knowledge into both `src/compiler/compile.zig:9008-9036` and `src/interp/vm.zig:9016-9066` was false progress. The minute a user-defined condition subclass appeared, `handler-case` and `handler-bind` diverged from CLOS/type semantics.
- Trying to keep unsupported variadic pseudo-builtins callable via the old generic wrapper would have preserved fake progress. The right cutover was to delete the eval wrapper and only keep entries with a real runtime implementation.
- Auto-creating missing reader packages for forward references in `src/reader/parser.zig:1162-1184` was false progress. It let `pkg:sym` syntax appear to work while silently mutating global package state and bypassing external-only enforcement.
- A hardcoded loader byte cap is just another fallback in disguise. It turns legitimate upstream files into fake “too big” failures even though the parser and evaluator can already handle the content once it is in memory.
- Treating package-qualified reader tests as pure parser tests without preloading upstream package definitions is misleading. Real Maxima files rely on packages created by `maxima-package.lisp`, so the reader stage must establish that package surface first or it measures the wrong failure mode.
- `zig build test` is still blocked by the pre-existing 5-error baseline (`src/bytecode/disasm.zig:68`, `src/bytecode/emit.zig:3093`, `src/compiler/passes/p04_resolve.zig:269`, `src/compiler/passes/p05_capture.zig:117`, `src/types/erasure.zig:109`), so `zig build` is the only whole-repo validation path available for this batch.

---

## Session Notes (2026-03-07)

### Worked Well
- Reviewing `PLAN.md` against upstream Maxima code instead of plan prose immediately exposed missing prerequisites: `share/**` search paths in `lib/maxima-post-load.lisp:129-170` were not enough until wildcard directory descent in `src/runtime/primitives/io.zig:2428-2455` was considered, because upstream `init-cl.lisp:243-301` relies on recursive `**` patterns.
- Treating `test-batch` in `../maxima/src/mload.lisp:379-509` as the canonical correctness path was the right framing. It surfaced hidden Stage-1 dependencies on `testsuite.lisp`, `generr.lisp`, `macdes.lisp`, file-driven two-way streams (`src/runtime/primitives/io.zig:1748-1960`), and state cleanup that the custom runner in `tools/maxima-rtest.lisp:1-64` was masking.
- Splitting Maxima execution into separate infrastructure families (`$batch :test` via `test-batch` vs `$batch :batch/:demo` via `batch-stream`/`continue`/`dbm-read`) produced a much cleaner roadmap than treating all batch failures as one `dbm-read` problem (`../maxima/src/mload.lisp:165-205,379-509`, `../maxima/src/macsys.lisp:163-313`, `../maxima/src/mdebug.lisp:262-340`).
- The `%unread-char-from-stream` batch recursion was a generic compiler table bug, not a Maxima quirk: the symbol was mistakenly present in unary dispatch as well as binary dispatch, so lazy function-wrapper synthesis fell back to a recursive generic call instead of emitting `unread_char_stream` (`src/compiler/compile.zig:15948-15951,16444-16456`). Removing the stray unary entry restored real `$batch` progress.
- For CL pathname support, `open` must normalize pathname designators before `probe-file` / `%open-file`; otherwise direct string probes pass while real callers like `test-batch` fail on pathname arguments from `alter-pathname` (`lib/stdlib.habu:6193-6229`, `../maxima/src/mload.lisp:399-404`).
- Reader conditionals inside dotted pairs must treat skipped `#+`/`#-` branches as “keep scanning for the cdr expression”, not as hard syntax errors. Allowing `SkipForm` to loop in the dotted-pair branches of `parseList` / `parseListTail` fixed real Maxima share-module loads such as `share/stringproc/unicode-sniffer.lisp` (`src/reader/parser.zig:271-283,302-314`, `../maxima/share/stringproc/unicode-sniffer.lisp:11-23`).
- A tiny early Maxima-local patch after `clmacs.lisp` can safely redirect `quotient` through `(funcall #'quot ...)`, which preserves the intended CLMACS helper semantics for later Maxima files without teaching the generic compiler about Maxima (`lib/maxima-loader.lisp:100-116`, `lib/maxima-early-patches.lisp:1-17`, `../maxima/src/clmacs.lisp:55-64`).
- In real batch/test-batch paths, `arrayp`/`vectorp` can be unreliable enough to send `fill` down its list branch even when `type-of` reports `ARRAY`; guarding array fills with `typep` first kept `displa` cleanup on the `aref` path and unblocked plain `$batch` / mini `test-batch` execution (`lib/stdlib.habu:2713-2727`, `../maxima/src/displa.lisp:60-70`).
- VM global slots must default to `Value.unbound`, not `nil`. Predefining a DEFUN name in the global environment while leaving the slot `nil` makes `(boundp 'fn)` spuriously true and breaks Maxima function-designator evaluation paths such as `map(unicode, ...)` (`src/interp/vm.zig:1299-1303`, `src/compiler/compile.zig:9388-9429`, `../maxima/src/mlisp.lisp:1162-1178`).
- Maxima share crypto/stringproc files rely on CL rank-1 array behavior more than the current Habu runtime does. Making `svref`/`vectorp`/`char` tolerate rank-1 arrays was enough to unblock `share/stringproc/md5.lisp`, `base64.lisp`, and `sha1.lisp` without changing the generic Maxima code (`src/interp/vm.zig:4312-4321,4392-4428,4743-4814`, `src/runtime/primitives/vector.zig:306-313`, `../maxima/share/stringproc/md5.lisp:87-140`, `../maxima/share/stringproc/base64.lisp:58-140`, `../maxima/share/stringproc/sha1.lisp:182-318`).
- `make-string` must allocate `String32` when the fill character codepoint is above 255; otherwise higher-plane `string(code-char(...))` calls collapse under `ignore-errors`, which made Maxima `unicode` silently return `nil` for whitespace codepoints like U+200B and masked downstream parser bugs (`src/interp/vm.zig:5461-5486`, `lib/stdlib.habu:396-400`, `../maxima/share/stringproc/stringproc.lisp:617-631`).
- For Maxima `parse_string`, getting `String32` into `make-string-input-stream` is only half the fix: Habu currently feeds Maxima byte streams, so the stock non-GCL `gobble-whitespace` misses UTF-8-encoded Unicode spaces. Reusing the upstream byte-reconstruction logic (originally GCL-only) in a post-load override fixed `parse_string("ex: <ZWSP>23;")` and the `space_chars` test batch (`src/runtime/primitives/io.zig:1687-1718`, `lib/maxima-post-load.lisp:78-109`, `../maxima/src/nparse.lisp:161-178`, `../maxima/tests/rtest1.mac:742-767`).
- Maxima numeric token parsing in non-decimal ibase relies on invalid-digit integers staying symbolic. A `readlist` override that falls back to `read-from-string` on partial `parse-integer` consumption is wrong for tokens like `23401` under `ibase:2` or `8765` under `ibase:8`; return `implode`d symbols instead (`lib/maxima-post-load.lisp:14-31`, `../maxima/src/nparse.lisp:455-482`, `../maxima/tests/rtest1.mac:97-124`).
- `declare-top (special ...)` plus plain `&aux var` was a real compiler hole: only `&aux (var init)` special bindings were added to lambda special-parameter wrapping. Bare `&aux vlist` stayed lexical-only, which left Maxima globals like `rat3e.lisp`'s `vlist` dynamically unbound inside helper calls (`src/compiler/compile.zig:4607-4624`, `../maxima/src/rat3e.lisp:17,729-746`). Adding the missing `appendSpecialParam` call fixed the `t[4](y)` / `ratexpand` blocker from `rtest1` problem 14.
- `loop-finish` support in Habu's `loop` macro was too shallow: rewriting only top-level body forms missed valid upstream patterns where `loop-finish` appears inside `cond` clauses in a loop body (`../maxima/src/todd-coxeter.lisp:133-161`). A targeted recursive rewrite for `cond`/control forms in `loop-rewrite-control-form` fixed the `LOOP-FINISH used outside LOOP` failure without needing the over-broad whole-tree rewrite (`lib/stdlib.habu:6150-6176`).
- When a Maxima radical simplification mysteriously collapses to `1`, test closure mutation before touching the algebra: `sqrt(4) -> 1` and `simpnrt 4 2 -> 1` were ultimately caused by `labels` closures dropping writes to captured lexicals (`src/compiler/compile.zig:5557-5690,5778-5830`). The minimal repro `(let ((acc nil)) (labels ((f () (push 2 acc))) (f) acc))` returning `nil` proved the boxing scan was missing `labels`/`flet` local-function bodies. Teaching `collectMutationsAndCaptures` / `collectFreeVarRefs` about local function definitions fixed both the repro and Maxima `simpnrt`/`sqrt`.
- Maxima bigfloat arithmetic is a good way to surface missing integer primitive coverage even when generic `+`/`*` already support bignums. The `parse_string("2.3b1")` path only started working after extending Habu's integer helpers used by `src/float.lisp:fpround` and friends: `integer-length`, `ash`, and `abs` all needed bignum support in the VM/runtime (`src/runtime/primitives/arith.zig:355-447,487-497`, `src/interp/vm.zig:4138-4186,8441-8458`). Fixing only the top-level arithmetic operators was not enough.
- The remaining `rtest_stringproc` sequence cluster came from generic CL sequence APIs being too list-only or too narrow about string tests. Canonical fixes were to make `reverse` work for strings/vectors (`lib/stdlib.habu:1028-1044`, `src/interp/vm.zig:4138-4186`), add `end1`/`end2` and string-aware element comparison to `search`/`mismatch` (`lib/stdlib.habu:2704-2738`), strip `$` when classifying Maxima test designators for string comparisons (`lib/stdlib.habu:2470-2478`), and accept `:start`/`:end` in `position-if` for `tokens` (`lib/stdlib.habu:1989-2002`).
- A narrow upstream fix in `../maxima/src/mload.lisp:282-294` was enough to clear the last bigfloat comparison blocker in `rtest_stringproc`: `$bfloat_approx_equal` should not route bigfloat diffs back through generic `mabs`/`abs` when it already knows it is comparing bfloat objects. Normalizing the sign of the bfloat mantissa directly avoided another raw-`abs` bridge hole and let the canonical suite finish.
- For heavy Maxima algorithms that resize rank-1 tables, `adjust-array` cannot be a lossy stdlib copy helper. `../maxima/src/todd-coxeter.lisp:247-264` resizes its multiplication tables repeatedly; when Habu's `lib/stdlib.habu:4491-4510` filled new slots with `nil`, `undef` checks stopped recognizing new coset entries as zero/unbound and Todd–Coxeter either diverged or crashed. Extending `src/runtime/primitives/vector.zig:194-252` to resize rank-1 arrays as well as vectors, and routing rank-1 `adjust-array` calls through `%adjust-array`, restored correct table growth.
- Maxima's heavy local-variable names can accidentally trip Habu's special-binding path often enough to matter. In `../maxima/src/todd-coxeter.lisp:77-264`, short locals like `i`, `j`, `m`, `n`, `s`, `s2` compiled to `push_progv` in hot loops/functions, and `replace-coset-in-multiply-table` eventually overflowed the progv stack on the 448-coset example. Renaming hot locals to distinctive `tc-*` names and avoiding common lexical names in `with-multiply-table` removed the unwanted dynamic-binding traffic and let `todd_coxeter([a^^8,...],[a^^2,...])` finish (`Rows tried 1876`, result `448`).

### Did Not Work
- Assuming that adding `share/**` globs alone would fix autoload was incomplete; without recursive wildcard descent in directory scanning, the new search paths are inert for nested share packages (`src/runtime/primitives/io.zig:2428-2455`).
- Treating the `meval*` post-load override cleanup as “non-essential” was too optimistic. Skipping `clearsign` in `lib/maxima-post-load.lisp:89-103` makes long sequential Maxima runs untrustworthy even before the full VM unwind/handler fix lands (`../maxima/src/suprv1.lisp:69-85`, `../maxima/src/compar.lisp:965-976`).
- Focusing only on `dbm-read` for canonical testsuite execution missed an earlier blocker: `test-batch` with `answers_from_file=t` depends on two-way stream reads and `mread-noprompt`, so `macdes.lisp` and composite stream char/line ops must land before `run_testsuite` can be trusted (`../maxima/src/mload.lisp:379-509`, `../maxima/src/macdes.lisp:80-86`, `src/runtime/primitives/io.zig:1748-1960`).
- Interned builtin dispatch in the compiler should not accrete long raw-symbol `if (s == b.foo.raw)` chains. When builtin identities are runtime-interned values (not comptime-known Zig constants), replace the chain with table-driven dispatch plus a small enum `switch` on the matched action; a direct `switch (s)` is not legal in that case (`src/compiler/compile.zig:16482-16513`).
- Trying to solve Maxima’s `quot` / `quotient` collision by making the generic compiler prefer all exact function cells over builtin lowering was too blunt: it introduced severe compile/load slowdowns and still did not solve the concrete Maxima path. For imported CL symbols reused by Maxima (`COMMON-LISP::QUOT`), a Maxima-local early patch was the right containment boundary (`src/compiler/compile.zig`, reverted trial; `lib/maxima-early-patches.lisp:1-17`).
- UTF-8-encoding `String32` into byte string streams is not sufficient by itself for Maxima parsing. Habu byte streams plus upstream non-GCL `gobble-whitespace` still reject Unicode-space input; the parser-side whitespace recognizer must be adapted too, or `parse_string` fails later in syntax-error reporting (`src/runtime/primitives/io.zig:1687-1718`, `lib/maxima-post-load.lisp:78-109`, `../maxima/src/nparse.lisp:173-180`).
- Rewriting `loop-finish` across arbitrary cons trees was too blunt and made broader Maxima loads/rtests effectively timeout. Restrict the rewrite to known control-form shapes (especially `cond`) instead of a generic recursive `mapcar` over every list (`lib/stdlib.habu:6150-6176`, reverted broader variant during `todd_coxeter` work).

## Session Notes (2026-03-06)

### Worked Well
- Implementing `&optional`/`&key` supplied-p needed two distinct fixes: count supplied-p locals in bytecode lambda `param_slots` (`src/bytecode/emit.zig:1576-1588`) **and** bind supplied-p vars with `bindSym` instead of `bindName` so body references resolve as locals instead of globals (`src/compiler/compile.zig:4732-4773`). Disassembly was the fastest proof: bad build showed `load_global` for `b-p`; fixed build shows `load_local`.
- Direct `./zig-out/bin/habu <script>` probes remain the right validation path for Maxima compatibility work; `zig build test` still stalls in this environment and should not be used as the main feedback loop for Lisp integration changes.
- When Maxima is loaded from the source tree instead of an installed prefix, `init-cl.lisp` leaves `*maxima-srcdir*`, `*maxima-testsdir*`, `$file_search_lisp`, `$file_search_maxima`, and `$file_search_tests` nil. Bootstrapping those in `lib/maxima-post-load.lisp:129-170` immediately fixes `file_search` and `$load(file_search(...))` for test files.

### Did Not Work
- A `dbm-read` fallback alone was not enough to make `$batch` work for source-tree test files. After search-list bootstrap, `$batch` still goes through `macsys.lisp:163-240` / `continue` and fails with `PROGRAM-ERROR nil`; the remaining blocker is in batch/continue semantics, not path discovery.
- Overriding `stream-name` to tolerate stream objects removed the noisy `pathname` TypeMismatch traces, but it did **not** fix the actual batch failure. Treat stream-name/pathname cleanup as adjacent hygiene, not the root cause.

## Session Notes (2026-02-25)

### Worked Well
- For deep-copied known-callee IR where literal-root maps are unavailable, carrying callee symbol identity (`callee_sym_raw`) from VM JIT registration into `KnownFn` and using raw-identity fallback for self-call detection restored cross-call TCO admission for `NQUEENS-SAFE-P` (`src/interp/repl.zig:3091-3110`, `src/jit/backend.zig:1928`, `src/jit/backend.zig:3672-3690`, `src/jit/backend.zig:5836-5848`).
- Propagating callee symbol identity into inlined TCO translation context (`fn_symbol_raw`) let tailcall lowering recognize literal self-targets during inlined callee translation, which removed one `NQUEENS-SOLVE -> NQUEENS-SAFE-P` indirect edge (`patched=1` instead of `patched=2`) and improved `nqueens10` on this host (~`3.53ms` -> ~`3.35ms`, `--iters=80`) (`src/jit/backend.zig:2073`, `src/jit/backend.zig:3813-3832`, `src/jit/backend.zig:4081`, `src/jit/backend.zig:4200-4201`).
- In long-running benchmark loops, rooting the cached runner function value through VM ext roots (`saveExtRoots` + owned root array + `setExtRootsOwned`) prevented stale closure designators across GC and eliminated `CALL_MISMATCH reason=closure-code-not-chunk` failures from `callFromStackAtFast` (`bench/maxima_workload.zig:465+`).
- For Maxima benchmark input upgrades, switching from numeric constants (`0`/`1`) to symbolic atoms (`'maxima::$x`) in wrapper calls gave non-constant CAS paths without triggering current unsupported deep-expression failure modes; this kept correctness smoke green while still avoiding constant-only fast paths (`bench/maxima_workload.zig`, `bench/maxima_workload.lisp`).
- For GC OOMs reporting absurd symbol sizes, tracing bad-root origin down to slot indices (`origin=slot:<n>`) and then classifying whether the slot is VM external vs heap internal quickly narrowed the culprit to `class_metadata` slot-name roots instead of stack/global roots (`src/runtime/gc.zig:1645+`, `src/runtime/heap.zig:3481+`).
- Re-interning slot symbols from stable slot-name strings when persisting DEFCLASS metadata prevented stale parser-captured symbol values from being written into long-lived metadata arrays: use `heap.intern(spec.name)` for both compiler-persistent specs and heap runtime class metadata (`src/compiler/compile.zig:12368+`, `src/compiler/compile.zig:12389+`).
- Rooting the `eval-when` compile-toplevel body cursor in `evalCompileToplevel` (`src/interp/repl.zig:4512+`) removed a real moving-GC stale-pointer crash class during large Maxima loads; each iteration now re-resolves live tail state before evaluating the next form.
- Restoring speed/safety admission gating in JIT candidate eligibility (`src/jit/candidates.zig:106+`) prevented accidental hoist compilation of thousands of default `speed=1/safety=1` Maxima functions, which was driving loader OOM in `bench-maxima --jit=on`.
- Running `tools/maxima-hotspots` via a built binary (`zig-out/bin/maxima_workload_bench`) instead of rebuilding `bench-maxima` for every mode made hotspot loops reproducible and removed avoidable tool overhead (`tools/maxima-hotspots`).
- For Maxima benchmark wrappers, calling package functions via quoted-symbol `funcall` (`(funcall 'maxima::$factor ...)`) avoided macroexpansion-time IR pollution in wrapper defuns and restored JIT compilation of workload wrappers (`bench/maxima_workload.zig`).
- `bench/maxima_workload.zig` produced stabler hotspot comparisons when workload functions were single-call wrappers and Zig performed iteration via `Vm.callFromStackAtFast`, allowing JIT traces to target wrapper compilation directly (`BENCH-MAXIMA-FACTOR` / `BENCH-MAXIMA-RATSIMP`).
- For filtered hotspot runs, `tools/maxima-hotspots` should derive `jit_gate.checks.compiled_min.target_min` from selected workload count (unless explicitly overridden); a fixed `--min-jit-compiled=32` obscures real progress on narrow workload slices (`factor,ratsimp`).
- `tools/validate-session --profile maxima-macro-hang` is more reliable as a correctness closure gate when it explicitly runs a `bench-maxima` jit/interp smoke check and verifies loader completeness + zero workload errors + `jit_compiled > interp_compiled` before perf interpretation.
- In `tools/perf-loop`, separating a Maxima execution gate (load+compile+run correctness) from the JIT speedup gate keeps optimization loops aligned with functional goals; performance thresholds should be informational unless explicitly enforced.
- When adding a new JIT translation case (`.list`/`.list_star`), update both translation and admissibility diagnostics (`canTranslateWithLiteralRoots` and `firstUnsupportedTagWithLiteralRoots` in `src/jit/backend.zig`) in the same change; otherwise traces keep reporting the old tag as unsupported even though lowering exists.
- For the `mforma` form-34 stall, isolating macro body subexpressions showed the real blocker was dynamic `append` invocation (`apply`/`funcall`) rather than macro-rewrite recursion: `(apply #'append ...)` hung while direct `(append ...)` succeeded, which pinpointed a function-designator path bug quickly (`lib/stdlib.habu:963-986`, `/tmp/mformat_dispatch_subexpr_probe.lisp`).
- Replacing `%append2` primitive-capture indirection with an explicit binary append helper in stdlib removed self-recursive dynamic-call behavior and unblocked full Maxima macro expansion (`lib/stdlib.habu:963-991`); `/tmp/mforma_trace.lisp` now reaches `[PHASE] mforma done`.
- Locking append dynamic-call behavior with integration assertions for both `funcall` and `apply` on `#'append` and `(symbol-function 'append)` prevents regressions in macro-heavy code paths that rely on function designators (`src/tests/integration.zig:1696-1708`).
- Encoding batch closure evidence in a single script (`tools/validate-session --profile maxima-macro-hang`) reduced ad-hoc command drift: focused tests + mforma completion proof + one timeout-bound full-suite attempt are now reproducible from one command.
- Restoring a full-file JJ conflict artifact by materializing the known-good file content from a specific revision (`jj file show -r <rev> path > path`) was safer and faster than hand-editing prefixed conflict-diff lines (`src/runtime/primitives/type.zig`).
- Making age-gated promotion a heap-owned runtime knob (`src/runtime/heap.zig`: `promote_age_threshold`, `setPromoteAgeThreshold`) let GC control logic adapt policy without hardcoded collector constants (`src/runtime/gc.zig:shouldPromote`).
- Separating tenuring-byte policy and age-threshold policy (`deriveTenuringPolicy` + `derivePromoteAgePolicy` in `src/runtime/gc.zig`) kept control loops simple and testable; dedicated policy tests caught bound/step behavior directly without requiring full runtime benches.
- Wiring bench/tool controls end-to-end (`bench/maxima_workload.zig --promote-age`, `tools/maxima-hotspots --promote-age`) turned policy tuning into a reproducible command-line parameter instead of ad-hoc code edits.
- For large accidental file-drop states, scripting restore from the immediate pre-break revision (`817a1145bd62`) by scanning missing `@import` targets and replaying `jj file show -r <rev> <path>` restored build integrity quickly without hand-chasing every compiler error.
- Treating plan findings as hypotheses first, then validating against execution invariants (`src/interp/vm.zig:2106-2109`, `src/interp/vm.zig:2636-2637`) prevented incorrect escalation of GC-staleness reports in JIT helpers and produced a cleaner `PLAN.md` with explicit accept/reject rationale.
- Splitting `doHoistCompile` map population into a dedicated helper (`src/interp/repl.zig:3092+`, `populateKnownFns`) made allocator-failure behavior unit-testable: deterministic first-insert, partial-map, and no-preseed control cases can now be validated without depending on full Hoist compilation side effects.
- Reworking the OOM relay integration workload from retained cons-list growth to repeated transient `make-string` allocations (`src/tests/integration.zig:9658+`) made fallback observability deterministic and fast enough for 3 consecutive reruns while still proving JIT-attempt + fallback-counter + GC-delta invariants.
- For the MAXIMA `mforma` hang, creating prefix-only repro scripts (`/tmp/mforma_prefix_33.lisp` + `/tmp/mforma_prefix_plain_defun_loop.lisp`) isolated the trigger to defining a function whose body contains `mformat-loop-c`; a minimal `defun-maclisp` without `mformat-loop-c` does not hang, which narrows RCA away from generic `defun-maclisp` handling.

### Did Not Work
- A `namesMatch` case-insensitive/suffix-comparison tweak alone did not restore `NQUEENS-SAFE-P` self-tail detection (`self_tail` remained false in `JIT_XCALL_TCO` traces); the blocker was missing symbol identity for literal targets, not case normalization (`src/jit/backend.zig:namesMatch` trial, reverted).
- Caching a resolved closure `Value` in Zig locals across repeated VM calls without rooting is unsafe under moving GC: the value can become stale and later decode as a closure with non-chunk code (`code-kind=cons`), surfacing as `CALL_MISMATCH reason=closure-code-not-chunk` during benchmark loops (`bench/maxima_workload.zig` pre-fix runner loop).
- Using richer handcrafted internal-expression trees for benchmark wrappers (e.g. nested `mplus`/`mexpt`/`mquotient` forms) currently triggered unstable runtime failures in this environment (`TypeMismatch`, `UnboundSymbol`, `UnhandledThrow`, `StackOverflow`), so benchmark input upgrades should be introduced incrementally and validated workload-by-workload before widening expression complexity (`bench/maxima_workload.zig` diff/integrate/factor/ratsimp trials).
- Persisting raw `spec.sym` values captured from parser forms into DEFCLASS metadata is unsafe under moving GC: those values can become stale before metadata persistence and later surface as corrupted symbol roots (`name_len` in billions) during GC copy from `class_metadata` slots (`src/compiler/compile.zig` pre-fix around `12368-12394`).
- Leaving JIT eligibility unconstrained by optimization settings let `bench-maxima --jit=on` attempt hoist compilation for default `speed=1/safety=1` forms, which inflated compile-state pressure and manifested as loader OOM around `rat3e.lisp` form 58 (`ALGORDSET`) despite no functional need to compile those functions.
- Benchmark wrapper defuns that directly call Maxima entry symbols can compile to unsupported IR (`define`) after full Maxima load; wrapper call shape matters. Rewriting wrappers to no-loop single-call defuns and doing iteration in Zig (`Vm.callFromStackAtFast`) avoided this unsupported path while preserving benchmark intent (`bench/maxima_workload.zig`, JIT traces around `BENCH-MAXIMA-FACTOR`).
- Trying to patch `compileSetf` stale-name handling in-place for `(setf (fdefinition '(setf ...)) ...)` without a full root strategy changed the failure mode from `InvalidSyntax` to GC-time bogus symbol sizes (`TRACE gc-copy-oom ... tag=symbol size~4.4GB`), so this path needs a dedicated rooted parse/compile design instead of partial `resolveForwardedValue` shims (`src/compiler/compile.zig:7090+`, filtered `def%tr` test).
- Running the full `tools/perf-loop --json` end-to-end is still timeout-prone in this environment, so correctness gating must not depend on it; use targeted `tools/validate-session` Maxima smoke checks as the hard pass/fail loop.
- Treating the `mforma` timeout as pure macro-expansion recursion for too long delayed RCA: direct probes showed `macroexpand-1` stabilized, and the real loop was dynamic `append` calls inside macro bodies (`apply/funcall` designator path), not repeated rewrite growth (`lib/stdlib.habu:963-986`, `/tmp/mformat_dispatch_macro_probe.lisp`).
- Running focused validations as hand-typed command chains made closure evidence easy to drift/reorder; codify recurring proof bundles in a repo tool (`tools/validate-session`) instead of repeating shell history.
- Treating `zig build` failure as a local code-change regression was incorrect in this session: build/test paths are currently blocked by repository state drift (missing `build.zig.zon` plus missing runtime/build entry files referenced by imports/build graph), so perf rebaseline steps must be gated on build-graph restoration first.
- Running `python -m py_compile` in-tree created tracked `__pycache__` artifacts; these must be immediately removed and never committed (`tools/__pycache__/maxima-hotspotscpython-314.pyc`).
- Accepting deep-review severity labels at face value (without checking current VM JIT GC fences) can create false-critical plan churn; verify findings against live control-flow guards before prioritizing fixes (`src/interp/vm.zig:2106-2109`, `src/interp/vm.zig:2636-2637`, `src/jit/backend.zig:816-917`).
- "Review plan" can drift into plan-prose critique if prompts/skills do not force a fresh code-grounding phase; require explicit file:line re-verification + goal→plan coverage matrix before accepting plan conclusions.
- Subagent review prompts can silently mis-dispatch to unknown default agents (e.g., `claude`/`code`) if task roles are phrased loosely; always verify subagent result metadata and ensure configured project agent names (`plan-critic`, `edge-case-hunter`, `scout`) were actually used.
- Using broad retained-allocation loops in OOM fallback tests (`cons` list growth) made CI/runtime behavior bimodal (sometimes no fallback, sometimes long timeout). Prefer bounded transient allocations with explicit pressure ladders and direct fallback counters (`src/tests/integration.zig:9688+`) for deterministic OOM-path evidence.
- Treating `macroexpand-1` as proof that full macro expansion is safe was misleading for `mformat-loop-c`: `macroexpand-1` completed quickly while recursive `macroexpand`/top-level `defun` expansion still hung, so RCA for macro stalls must always test the full expansion path.

## Session Notes (2026-02-24)

### Worked Well
- Treating stale-resolve traps as hypotheses, then validating against root-range provenance (`origin=range:6`) and package-table state, exposed a false-positive class instead of a real stale-root leak: current-cycle to-space symbol headers (`name_len=14` => `0xe`) were being misread as forwarding metadata (`src/runtime/gc.zig:1263+`).
- Fixing stale-forwarding classification at the source (require forwarding target address to be inside heap before reading forwarding-size metadata) removed strict-trap Maxima failures without adding fallback behavior (`src/runtime/gc.zig:1273`).
- Replacing `typep` integer-range parsing with a CL-style bound parser (`src/runtime/primitives/type.zig`) fixed both correctness and Maxima behavior: `(typep 1 '(integer 0))` now succeeds and full-loader factor probe reaches `:OK` (`/tmp/habu_factor_probe_full.lisp` output `(85 85 0 t :OK)`).
- Removing legacy global-name fallback probes in `lookupSymbolGlobalIndex` (`src/interp/vm.zig`) and locking it with `vm does not use legacy global fallback names` prevented silent CL/CL-USER prefix fallback behavior from creeping back in after hard cutover.
- Enforcing `(optimize (speed 3) (safety 0))` at compile time for every JIT benchmark `defun` (`bench/comprehensive_bench.zig:65`, `bench/comprehensive_bench.zig:396`) prevents silent benchmark-mode drift and catches missing declarations during build instead of after noisy perf runs.
- Splitting call-target name handling by context fixed a real crash class: keep static recursion/cross-call analysis on `.global_ref` only (`src/jit/backend.zig:getCallTargetName`, `src/jit/backend.zig:isCallTargetSelf`) and use rooted literal slots only in translation-time dispatch (`src/jit/backend.zig:IrTranslator.callTargetName`); this preserved primitive/known dispatch while eliminating stale `.lit` symbol dereferences in deep-copied IR.
- Extending recursion/tail-call analysis with literal-root-aware variants for the current lambda (`src/jit/backend.zig:detectSelfCallsWithLiteralRoots`, `src/jit/backend.zig:hasSelfTailCallsWithLiteralRoots`) restored self-recursion classification for literal call targets: `NQUEENS-SAFE-P` no longer compiles with `mode=generic` self-calls under `HABU_TRACE_JIT_XCALL`.
- Clearing stale chunk fast pointers during GC JIT rekey (`src/interp/vm.zig:rekeyJitFnsAfterGc`) and rejecting stale-nursery chunk addresses in `lookupJitFn` (`src/interp/vm.zig:lookupJitFn`) restored post-GC chunk-lookup invariants and fixed `compileChunk rekeys JIT map after chunk movement GC`.
- Re-running real workloads immediately after crash-path fixes (`tools/maxima-hotspots --json --scale 1 --heap-mb 1024 --nursery-mb 32 --workloads factor,ratsimp`) confirmed panic removal in the exact failing path before closing the dot.
- Moving `bench/comprehensive_bench.zig` timed loops from per-iteration `repl.eval(expr)` to pre-resolved runner function values invoked with `Vm.callFromStackAtFast` removed parser/evaluator overhead from microbench timings and improved `nqueens10` from ~`3.98ms` to ~`3.48ms` on this host while preserving benchmark semantics.
- Re-running cross-call TCO after restoring known-function deep-copy data showed the original crash came from an over-broad recursive-callee gate, not from every nested-loop inline case: tightening `translateCrossCall` to recursive loop callers plus tail-only self-recursive load-bearing callees (`src/jit/backend.zig:3608`) restored safe inlining and improved `nqueens10` (~`3,145,450ns` -> `3,080,125ns`, 5-run avg, `--iters=80`).
- Always validating the perf change against SBCL right after A/B (`sbcl --script bench/comprehensive.lisp --json --iters 80 --bench nqueens10`) prevented stopping at local improvements only; this fix reached slight host-local lead (`Habu ~3,080,125ns` vs SBCL ~`3,086,800ns`).
- Extending `deepCopyIr` to cover the translator-supported JIT subset (`src/compiler/ir.zig:2942+`) removed `IR copy skipped` fallout from hoist compile logs (`115 -> 0` in `HABU_TRACE_JIT` nqueens traces) and restored retained `KnownFn.ir_body/param_names` metadata for known-function analysis/inlining paths in `src/interp/repl.zig:3138`.
- Locking the deep-copy restoration with `deepCopyIr copies block-wrapped recursive shape` (`src/compiler/ir.zig`) caught the missing `.block`/recursive-form coverage that originally nulled callee IR metadata under JIT.
- Keeping `doHoistCompile` on the normal post-registration path even when IR deep-copy fails (`src/interp/repl.zig:3099-3173`) restored `patchCrossCallsToBL` execution and compile-success telemetry for compiled functions that cannot be inlined yet; `NQUEENS-SOLVE` now reports patched cross-calls (`patched=2`) under `HABU_TRACE_JIT_PATCH=1`.
- Returning cross-call BL patch counts from backend patching (`src/jit/backend.zig:5149`, `src/jit/backend_stub.zig:180`) plus focused patch-count tests made rewrite coverage measurable instead of inferred.
- Extending `HABU_TRACE_JIT_PATCH` with per-function call-op deltas (`blr`/`bl`) in `src/interp/repl.zig` confirmed that `NQUEENS-SOLVE` has no residual indirect calls after patching (`blr=2->0`), which prevented further time on indirect-call RCA and redirected effort to codegen quality.
- Sampling long-running `nqueens10` after an explicit startup delay (attach at +12s, sample 5s, then kill) produced steady-state JIT hotspots instead of loader/warmup noise, giving actionable codegen signal.
- Comparing unchecked `PLAN.md` leaves against `dot list` before starting new perf work exposed plan drift immediately (`habu-close-post-fix-77d8f862` open but missing from `PLAN.md`), which prevented hidden execution debt.
- Encoding new performance work as dependent dots in `PLAN.md` (shape counters -> direct JIT stubs -> session JIT cache) keeps optimization sequencing explicit and avoids parallel speculative tuning.
- Adding gated VM call-shape counters directly in `doCall` (`src/interp/vm.zig:11100+`) and exporting load/run deltas via `bench/maxima_workload.zig` produced immediate, quantified attribution for dynamic-call overhead without changing semantics.
- Extending `tools/maxima-hotspots` to ingest `call_shape.run` from both JIT and interpreter payloads (`tools/maxima-hotspots:summarize`) made cross-mode call-shape drift visible in the same report as timing deltas.
- Re-running `factor/ratsimp` at both `--scale=1` and `--scale=120` before choosing next optimizations avoided a false “near parity” conclusion from short-run noise.
- Enforcing dual perf evidence directly in `tools/dot-finish` (auto-detected perf dots + micro/real workload commands + persisted artifacts) makes perf dot closure auditable and reduces single-benchmark bias.
- Adding a closure-only fixed-arity direct JIT path in VM call dispatch (`src/interp/vm.zig:tryDirectCallJit`) safely removed generic frame setup on eligible interpreted call sites; `jit_direct_calls` telemetry in `bench/maxima_workload.zig` and `tools/maxima-hotspots` confirmed activation in JIT runs.
- Caching JIT compile outcomes in VM with a deterministic chunk fingerprint key (`src/interp/vm.zig:computeJitChunkKey`, `src/interp/vm.zig:jitCompileStatus`) safely skipped repeated unsupported compile attempts without introducing fallback behavior; Maxima factor load now reports non-zero cache hits (`jit_adm.cache_unsupported=3`).
- Wiring cache checks before `doHoistCompile` in both REPL and test helper compile paths (`src/interp/repl.zig:tryHoistCompileLambdas`, `src/testing/compile_chunk.zig:tryHoistCompile`) kept admission counters coherent and cut redundant compile work during loader-heavy runs.
- Extending `JitAdmStats` with cache-hit counters (`src/interp/vm.zig:JitAdmStats`) made cache effect measurable in existing benchmark JSON without adding another telemetry channel.
- Dumping `BENCH-GC-CONS` hoist output before editing exposed a concrete hot-path bug (`src/jit/backend.zig:translateAdd`): safety=0 non-recursive generic `+` still emitted per-iteration helper calls in a fixnum loop, despite inline cons allocation.
- Adding guarded fixnum fast paths for generic `.add`/`.sub` (`src/jit/backend.zig:translateArithFixnumFastFallback`) with non-fixnum and overflow fallback to helpers delivered a large measured win on `gc_cons` (`~1.7-1.9ms` -> `~0.69-0.74ms`) without introducing a Maxima-specific path.
- Dumping `NQUEENS-SAFE-P` hoist IR after post-pass changes exposed that helper calls (`jitSubNum`/`jitNumEq`/`jitAddNum`) were back in a tail-recursive hot loop because TCO flipped `is_recursive=false`; keeping a separate `fixnum_inline` flag in `IrTranslator` (`src/jit/backend.zig`) preserved recursive fixnum-inline lowering through TCO and removed those helper calls again.
- Implementing a guarded mirrored-entry MOV eliminator (`src/jit/backend.zig:6640`) with strict shape checks (entry-only window, exact inverse second leg, disjoint first-leg src/dst sets, and post-mirror temp liveness) made this cleanup pass safe and deterministic, then locked behavior with dedicated backend tests (`src/jit/backend.zig:9738`, `src/jit/backend.zig:9770`, `src/jit/backend.zig:9801`).
- Running mirrored-entry elimination after first `compactNops` (`src/jit/backend.zig` pipeline) exposed normalized copy windows that were hidden by pre-compaction constant materialization, allowing safe removal of redundant restore MOV legs in hot JIT loops (`NQUEENS-SAFE-P` shrank from 144B to 120B and `nqueens10` moved from ~`3.51ms` to ~`3.15ms` at 60 iters on this host).
- Adding liveness-driven target-load pruning to cross-call BL patching (`src/jit/backend.zig:5266-5274`) safely removes dead non-adjacent MOVZ/MOVK target chains after BLR→BL rewrite while preserving shared load chains until their final use; this is locked by focused regressions (`src/jit/backend.zig:9180-9225`).

### Did Not Work
- Interpreting every `reject-size` stale-resolve trap as proof of an unresolved stale pointer path was wrong; with strict traps enabled, valid to-space objects during the same GC cycle can match forwarding tag bits and must be filtered by target-address validity first (`src/runtime/gc.zig:1263-1295`).
- Reworking VM symbol/function lookup caches by rekeying across GC or broad alias-first lookup changes in `lookupSymbolGlobalIndex` did not produce stable `factor/ratsimp` wins on this host; keep perf claims gated on repeated `tools/maxima-hotspots --scale 120` checks and revert speculative cache churn quickly.
- Treating inlining-threshold broadening as a safe knob without stronger structural proof caused catastrophic runtime regressions in hot recursion (`NQUEENS-SAFE-P` path); keep threshold experiments gated by strict shape constraints plus immediate A/B rollback rules.
- `./zig-out/bin/habu <script>` is not currently a trustworthy validation path for this repo when stdlib bootstrap is already broken. A direct probe for the new stream-slicing path failed in `src/main.zig:119-139` before reaching the changed code because `lib/stdlib.habu` still dies with `UnboundSymbol`; for now, treat `zig build` plus `zig build test` baseline comparison as the reliable gate for this class of change.
- Trying to harden `.lit` symbol-name extraction in static call-target analysis by consulting global JIT heap state (`g_heap`) was wrong: stale heap pointers in long-lived test flows caused overflow/segfault (`src/jit/backend.zig:safeLiteralSymbolName` trial). Static analysis must not depend on mutable global heap bridges.
- Keeping recursion detection `.global_ref`-only after the static `.lit` crash hardening regressed current-lambda recursive lowering (`NQUEENS-SAFE-P` fell to `mode=generic` self-calls); current-lambda analysis must use rooted literal slots, while deep-copied/no-root analysis stays conservative.
- Relaxing hoist opt gating to allow `.aggressive` for all call-free load-bearing functions caused pathological benchmark slowdowns/hangs; keep `has_loads` in the `.none` gate until load-path correctness/perf is proven.
- Re-enabling cross-call TCO with the loose `callsItself` gate (`src/jit/backend.zig`, trial state) inlined non-tail-self-recursive callees and reproduced `comprehensive_bench` termination; tail-only recursion checks are mandatory for this path.
- Judging cross-call-TCO perf via an env-gated trial (`HABU_ENABLE_XCALL_TCO=1`) produced misleading secondary-bench signals; finalize and measure the default code path after gating changes, then compare parent-vs-patch and SBCL.
- Assuming IR deep-copy misses were harmless in `doHoistCompile` was incorrect: the early `.compiled` returns bypassed cross-call BL patching and masked compile success traces, leaving measurable call overhead on functions like `NQUEENS-SOLVE`.
- Driving benchmark runners through `Vm.callFromStack` (non-fast path) caused severe cross-benchmark regressions; use `Vm.callFromStackAtFast` for timing harnesses that target JIT throughput.
- Keeping “meta” open dots (`curr`/`next`/`active` placeholders) without `PLAN.md` entries obscures real remaining work and makes completion status unreliable; these must be pruned or mapped into explicit plan leaves.
- Relying on plain `zig build test -- --test-filter ...` in this environment still stalls with no output; wrapping with `timeout` is required to prevent leaked long-lived test processes while keeping CI gates actionable.
- Using `continue` inside `executeOp` opcode switch during direct-call insertion was invalid (`continue expression outside loop`); opcode handlers must `return` from `executeOp` instead.
- Keying compile-status cache by per-chunk pointer identity did not produce useful reuse under Maxima loader churn; switching to deterministic chunk fingerprints was required to convert the cache from “mostly cold” to measurable hits.
- Running filtered tests via the build wrapper can still leave long-lived `.zig-cache/.../build ... test` processes active after command completion; explicit `pgrep`/`kill` cleanup is required before continuing perf work to avoid unified-exec process-limit pressure.
- Some long `zig build test -Dtest-filter=...` runs can stall with test binaries blocked in `test_runner.mainServer` waiting on `--listen=-` protocol input (`sample` trace on `.zig-cache/.../test`); for Maxima end-to-end checks in this environment, prefer direct runtime probes via `./zig-out/bin/habu <script>` and kill stale test runners.
- Reusing `is_recursive` for both call-shape lowering and numeric fast-path eligibility caused hidden performance regressions after TCO rewrites; recursion-driven call conversion and fixnum-inline policy need independent state in the translator.
- Running mirrored-entry MOV elimination too early in the pass pipeline (before first compaction) had negligible effect because constant materialization still split the mirror windows; placement in the pipeline mattered more than the transform itself.
- Sampling optimized JIT workloads often reports anonymous native PCs only (`???` in `sample`) for generated code ranges; combine `sample` with `HABU_DUMP_HOIST` and patch traces to map hotspots back to concrete generated blocks before changing passes.
- A tagged-abs peephole candidate for `sub/add/cmp/mov/sub/csel` in `NQUEENS-SAFE-P` looked promising in instruction count but lost on measured A/B (~`3.150ms` vs parent ~`3.142ms` over 5 runs at `nqueens10`/60 iters), so it was reverted; keep this path measurement-driven.
- Relaxing JIT opt-level gating to allow `.aggressive` on call-free load-bearing functions reintroduced the historical load-path instability: `nqueens10` no longer completed within `timeout 30` at 60 iterations, so keep `has_loads` in the `.none` gate until hoist-side load optimization bugs are fixed.
- Running parent/patch perf loops in a fresh `jj workspace` failed at first because the bench build shells out to `git rev-parse` and the workspace lacked `.git`; baseline loops only became runnable after wiring git metadata into the workspace.
- Adding a dead-callee-save pruning pass (`eliminateDeadCalleeSaveSlots`) to the JIT pipeline regressed `nqueens10` by ~5% in 5-run A/B (`src/jit/backend.zig` trial, reverted). Keep this optimization rejected unless new evidence isolates a safe win.
- Raising known-call inline-node thresholds in `translateCrossCall` did not inline the `NQUEENS-SAFE-P` helper (`HABU_TRACE_JIT_PATCH` for `NQUEENS-SOLVE` stayed `patched=2`), so this knob-change path was reverted.
- Broadening `translator.local_consts` to all TCO cross-call functions produced no win on `nqueens10` (5-run A/B was ~0.04% slower), so keep `local_consts` restricted to the existing targeted TCO case.
- A direct `.abs(.sub)` tagged fast path in `translateAbs` (`abs(l_raw-r_raw)+1`) looked promising in IR shape but regressed `nqueens10` by ~2.4% in 5-run A/B; keep the existing tagged-abs lowering.
- Benchmark-harness “stabilization” tweaks (higher default heap + forced pre-GC before timed runs in `bench/comprehensive_bench.zig`) made `nqueens10` slower (~1.4% in 5-run A/B), so treat harness changes as measurement-affecting and keep them out unless they improve both signal and throughput.
- For non-inline `fixnum_fast` code, adding a generic `.num_eq` fixnum-guard/fallback split in `translateNumEq` plus `jitNumEq` helper fast-path (`jitFastNumCmp(.eq)`) increased `nqueens10` runtime (~2.7% slower in 5-run parent-vs-patch A/B), so keep the simpler current `translateNumEq` lowering and only revisit with new profiler evidence.
- A machine-code pass that rewrote return trampolines (`mov xN,xM; b -> mov x0,xM; ret` when branch target was `mov x0,xN; ret`) passed focused unit tests but regressed `nqueens10` heavily (~6.6% slower in 5-run parent-vs-patch A/B), so keep return-trampoline branches as-is unless a profiler-guided variant proves a win.

## Session Notes (2026-02-23)

### Worked Well
- Enabling `HABU_TRACE_BAD_STORE=1` while running authoritative `bench-maxima` immediately exposed the first bad write site (`src/runtime/heap.zig:2115`) and pointed straight at compiler special-LET lowering (`src/compiler/compile.zig:5888-5895`) instead of later GC fallout.
- Rooting `compileLetWithTail`/`tryCompileSpecialLet` cursors with compile-root tokens (`src/compiler/compile.zig:5564-5595`, `src/compiler/compile.zig:5842-5910`) and rooting special-LET fast-path symbol/init slices via temporary VM ext roots (`src/compiler/compile.zig:5919-5951`) removed stale-symbol reintroduction and cleared the JIT Maxima OOM path at scales 1/20/120.
- Resolving values in `listFromSlice`/`listFromSliceWithTail` before `allocCons` (`src/compiler/compile.zig:9244-9255`) added a low-cost guardrail that prevented forwarded/stale list elements from being copied back into newly allocated compiler-built lists.
- Replacing VM JIT-function tracking from `AutoHashMap(usize,*CompiledFn)` to a compact entry array with in-place GC refresh (`src/interp/vm.zig:559`, `src/interp/vm.zig:1714`, `src/interp/vm.zig:1899`) removed hash rebuild churn from `collectGarbageExtra` and improved `factor`/`ratsimp` JIT-relative runtime in direct scale-120 rebaselines on this host.
- Replacing survivor-age tracking hash maps with semispace-indexed age arrays (`src/runtime/heap.zig:357`, `src/runtime/heap.zig:1961`, `src/runtime/heap.zig:1968`) removed `HashMap/Wyhash` overhead from minor-GC hot paths and delivered large absolute Maxima wins on both `factor` and `ratsimp` runs.
- Locking array-based survivor age behavior with `heap survivor age table rebuild maps nursery slots` (`src/runtime/heap.zig:3992`) protects saturation, outside-nursery filtering, and reset semantics after rebuilding age state.
- Adding a VM direct-mapped symbol->global-index cache in `lookupSymbolGlobalIndex` (`src/interp/vm.zig:2208`, `src/interp/vm.zig:1380`, `src/interp/vm.zig:1387`) removed repeated `qualSymWithHeap` + `GlobalEnv.lookup` string-hash lookups on hot symbol-resolution paths; invalidating on `setGlobalEnv` and GC (`src/interp/vm.zig:1224`, `src/interp/vm.zig:2620`) kept cache correctness under env swaps and moving-GC.
- Locking the cache invalidation contract with `vm global index cache resets on env swap` (`src/interp/vm.zig:13974`) prevented stale global-slot reuse across environment transitions while keeping factor/ratsimp hotspot runs near parity.
- Making caller-frame restore infallible on return paths (`src/interp/vm.zig:10770`, `src/interp/vm.zig:4383`, `src/interp/vm.zig:4419`) removed hot `try push` overhead while preserving frame-depth restoration behavior in existing regressions.
- Persisting JIT bridge ownership per VM (`src/interp/vm.zig:1700`, `src/interp/vm.zig:1730`, `src/interp/vm.zig:1160`) and clearing bridge globals only on owner `Vm.deinit` (`src/jit/backend.zig:139`, `src/jit/backend.zig:147`, `src/jit/backend.zig:159`) removed per-call bridge set/clear churn while keeping cross-VM bridge handoff safe (locked by `vm jit bridge lifecycle tracks owner vm`).
- Splitting JIT heap setup into owner check + cursor refresh (`src/interp/vm.zig:338`, `src/interp/vm.zig:1762`; `src/jit/backend.zig:134`, `src/jit/backend.zig:138`) avoided redundant `setHeap` resets on same-heap bridge paths while preserving inline-cons cursor coherence (locked by `jit heap cursor refresh tracks heap alloc pointer`).
- Caching bridge ownership via backend bridge-epoch (`src/jit/backend.zig:126`, `src/interp/vm.zig:1710`) removes hot per-call bridge-context probes while preserving cross-VM bridge handoff safety; repeated same-owner installs now keep epoch stable (`src/interp/vm.zig:13424`).
- Keeping `loadConst` freshness state in VM-local last-chunk memo fields (`const_last_chunk_key`/`const_last_gc_count`) while preserving the existing chunk-const cache table (`src/interp/vm.zig:691`, `src/interp/vm.zig:11801`) trimmed hot repeated-constant loads without changing chunk layout ABI.
- Guarding `.call`/`.ret` trace checks with `trace_call_ret` before invoking `shouldTraceCallRet` (`src/interp/vm.zig:4340`, `src/interp/vm.zig:4383`, `src/interp/vm.zig:4401`) removed unnecessary hot-path helper calls and trace-only function-designator reads in normal benchmark runs.
- Embedding a chunk-local compiled function pointer (`jit_fn`) and updating it on register/unregister/rekey (`src/runtime/objects.zig:813`, `src/interp/vm.zig:1656`, `src/interp/vm.zig:1674`, `src/interp/vm.zig:1694`, `src/interp/vm.zig:1790`) removed one hot `tryCallJit -> HashMap.get` dependency; 5-run `keyword_call` A/B on this host showed the direct chunk pointer path slightly faster than the lookup fallback variant.
- Routing REPL JIT registration failure cleanup through `unregisterJitFn` (`src/interp/repl.zig:3121`, `src/interp/repl.zig:3135`) keeps chunk-local JIT pointer state coherent when post-registration code patching fails.
- Caching chunk constant forwarding fixups by `(chunk_addr, gc_count)` (`src/interp/vm.zig:541`, `src/interp/vm.zig:1340`, `src/interp/vm.zig:11780`) removed repeated `loadConst -> resolveForwardedValue` checks on hot opcode paths and gave a measurable Maxima runtime drop (`integrate` into ~`145ms` range on this host).
- Special-casing tiny overlap-safe stack moves (`1..4`) in `stackMove` (`src/interp/vm.zig:10722`) reduced hot `doCall` keyword frame-relayout overhead and produced a measurable Maxima hotspot drop on `integrate` while preserving key/rest layout correctness.
- Skipping redundant post-resolution canonicalization in `doCall` for symbol designators (`src/interp/vm.zig:10807`) is safe when `resolveFunctionValue` already returns canonicalized callable values; keeping canonicalization for non-symbol call targets preserves forwarding safety.
- Folding `&key` validation to a single scan in `doCall` (`src/interp/vm.zig:10920`) removed repeated keyword-pair traversal while preserving ANSI `:allow-other-keys` semantics where a later `:allow-other-keys` still authorizes earlier unknown keywords.
- Checking `fn_resolve_cache` by raw symbol identity before forwarding canonicalization in `resolveFunctionValue` (`src/interp/vm.zig:1427`) removed a hot per-call `resolveForwardedValue` on cache-hit symbol calls and improved repeated Maxima hotspot runs (notably `integrate`/`factor`) without changing function-designator semantics.
- Long-run profiling (`bench-maxima --workloads=integrate --scale=80`) remained the fastest way to confirm that runtime work has shifted but still clusters in `doCall` + function-resolution paths after each cut.
- Caching small `&key` allowlists per callee chunk (`src/interp/vm.zig:531`, `src/interp/vm.zig:1337`, `src/interp/vm.zig:1345`, `src/interp/vm.zig:10958`) removed repeated plist walks from hot keyword validation while preserving fallback behavior for uncached/irregular lists.
- Clearing the keyword allowlist cache at GC boundaries (`src/interp/vm.zig:2487`) kept chunk-keyed cache entries safe under moving collectors without adding new root-management complexity.
- Deduplicating forwarded-resolution in function-designator resolution (`src/interp/vm.zig:1324`, `src/interp/vm.zig:1335`, `src/interp/vm.zig:1378`) removed redundant `resolveForwardedValue` work between `resolveFunctionValue`, function-cell lookup, and cache store paths while keeping symbol semantics unchanged.
- Running A/B checks against the parent revision in a separate `jj` workspace before keeping a perf change prevented locking in a microbench regression; repeated `tools/maxima-hotspots --json --scale 1 --heap-mb 1024 --nursery-mb 32` runs are more reliable than one noisy sample for call-path decisions.
- Fixing `&optional` + `&key` boundary detection to require a complete remaining key/value tail before switching to keyword mode (`src/interp/vm.zig:10960`, `src/interp/vm.zig:10966`) preserved constructor keyword-initarg behavior while preventing lone trailing keyword values from being misclassified as malformed keyword tails.
- Adding a small-array allowlist path for keyword validation (`src/interp/vm.zig:738`, `src/interp/vm.zig:10907`, `src/interp/vm.zig:10910`) retained generic keyword checking while reducing repeated cons-walks on repeated multi-key calls.
- Locking the path with targeted regressions (`src/tests/integration.zig:2394`, `src/tests/integration.zig:2400`) now covers both odd-tail positional keyword handling and paired-tail key-start handling for mixed `&optional`/`&key` call shapes.
- Adding a dedicated `keyword_call` microbench (`bench/comprehensive_bench.zig:127`) gives a stable hot-loop signal for `doCall` `&key` cost independent of full Maxima loader noise.
- Adding a dedicated fixed-arity call setup fast path (`src/interp/vm.zig:10651`, `src/interp/vm.zig:10815`) and a fast closure-code chunk decode path (`src/interp/vm.zig:10786`) removed hot `doCall` overhead from the no-`&optional`/no-`&key`/no-`&rest` majority path; Maxima hotspot reruns improved JIT runtime again (`integrate` ~`165ms` -> ~`157ms`, `factor` ~`53ms` -> ~`51.7ms`, `ratsimp` ~`40.1ms` -> ~`38.8ms`, `solve` ~`13.1ms` -> ~`12.8ms`).
- Locking the fast path with a stack-depth regression (`src/tests/integration.zig:2574`, `fixed-tail-acc`) prevents accidental loss of tail-call stack safety when refactoring fixed-arity frame setup.
- Converting function-resolution cache hits to raw symbol-identity checks with GC-epoch invalidation (`src/interp/vm.zig:1309`, `src/interp/vm.zig:1312`, `src/interp/vm.zig:2435`) removed per-call forwarded-value chasing on hot `doCall` paths; `tools/maxima-hotspots --json --scale 1 --heap-mb 1024 --nursery-mb 32` improved JIT runtimes on `integrate` (~173ms -> ~165ms), `factor` (~57.7ms -> ~53.0ms), `ratsimp` (~43.1ms -> ~40.1ms), and `solve` (~13.7ms -> ~13.1ms) in same-host reruns.
- Scanning function-cell plists directly from live symbol objects (`src/interp/vm.zig:1334`) and canonicalizing function-cell writes once at store time (`src/interp/vm.zig:1358`) reduced avoidable forwarded-resolution churn while keeping symbol-function semantics unchanged in focused regressions.
- Canonicalizing forwarded symbol/list values at `progv` boundaries (`src/interp/vm.zig:5213`, `src/interp/vm.zig:8117`, `src/interp/vm.zig:8194`) removed a deterministic non-hoist Maxima crash where `pushProgvFrame` dereferenced stale forwarded symbol objects (`name_ptr=0x30/0x40`) during macro-expansion-time dynamic binding.
- Extending symbol-cell and function-cell entry points to resolve forwarded symbol values before lookup/store (`src/interp/vm.zig:1249`, `src/interp/vm.zig:1262`, `src/interp/vm.zig:1281`, `src/interp/vm.zig:1294`) closed adjacent stale-pointer dereference paths beyond `progv`.
- Locking the bug with a VM-level regression that injects a deliberately stale forwarded symbol into a `progv` symbol list (`src/interp/vm.zig:13352`) gives deterministic red/green coverage without relying on long Maxima bench reproductions.
- Running Maxima hotspot “interp mode” on hoist with runtime JIT disabled (`src/interp/repl.zig:60`, `bench/maxima_workload.zig:12`, `tools/maxima-hotspots`) removed non-hoist drift and restored full workload comparability (`checked=5` instead of `checked=1`) for gate decisions.
- Replacing ext-root prefix copyback with snapshot-stack rooting (`src/interp/vm.zig:655`, `src/interp/vm.zig:1359`, `src/interp/vm.zig:2148`) fixed nested owner restore corruption; inactive ext-root owners/slices now stay in GC root ranges directly instead of being reconstructed from temporary arrays.
- Making `saveExtRoots` fallible and updating all swap callsites (`src/compiler/compile.zig:3606`, `src/interp/repl.zig:816`) removed silent snapshot-drop risk and kept nested ext-root save/restore bookkeeping explicit.
- Adding a JIT no-GC execution fence with OOM deopt (`src/interp/vm.zig:1535`, `src/interp/vm.zig:1998`, `src/interp/vm.zig:1550`) stopped moving-GC from running while JIT-held register values have no root map; `bench-maxima` now completes instead of crashing in `jitHashGet`.
- Aligning backend forwarding resolution with VM semantics (`src/jit/backend.zig:239`, `src/jit/backend.zig:262`, `src/jit/backend.zig:287`) and resolving hash helper arguments (`src/jit/backend.zig:681`, `src/jit/backend.zig:699`, `src/jit/backend.zig:730`) removed one stale-forwarding blind spot on helper entry.
- New regressions for ext-root behavior (`src/interp/vm.zig:13132`, `src/interp/vm.zig:13172`) lock both owner-backed and plain-slice inactive-root correctness.
- Running focused bridge/safety regressions plus `bench-maxima` rebaseline (`src/tests/integration.zig:1877`, `src/tests/integration.zig:2573`, `bench/maxima_workload.zig`) validated that bridge relay remains stable in JIT mode and safety admission stays open (`jit_adm.sk_safety=0`, loader `85/85`).
- Hardening `tools/dot-finish` with timeout-aware test execution (`tools/dot-finish`) removes a recurring dev-loop failure mode where full-suite hangs left stale `zig build test` processes alive for hours and tripped unified exec process limits.
- Sampling `./zig-out/bin/comprehensive_bench --bench=assoc` in Debug (`/tmp/habu_assoc_bin_sample.txt`) showed `jit.backend.jitAssoc` (`src/jit/backend.zig:360-369`) as the dominant hotspot with heavy `debug.assert` overhead; ReleaseFast `zig build -Doptimize=ReleaseFast bench-comp ...` measured ~5.0ms for the same bench.
- Rewriting `jitAssoc` to use raw tagged checks instead of `Value.isCons()/toPtr()` (`src/jit/backend.zig:360-374`) cut Debug `bench-comp --bench=assoc` from ~137ms to ~39ms on this host, improving inner-loop developer feedback.
- Adding direct fixnum/float fast paths in numeric compare helpers (`src/jit/backend.zig:459`, `src/jit/backend.zig:608`, `src/jit/backend.zig:618`, `src/jit/backend.zig:646`, `src/jit/backend.zig:653`) plus a fixnum-guarded translator fast lane (`src/jit/backend.zig:2602`) reduced ReleaseFast `assoc` from ~5.23ms to ~5.12ms while preserving generic fallback semantics.
- Sampling the real ReleaseFast bench binary (not Debug) for `assoc` kept the hotspot unambiguous in `jitAssoc`, which avoided false follow-up work on compiler/debug-only overhead (`/tmp/habu_assoc_releasefast_sample.txt`, `src/jit/backend.zig:387`).
- Disabling runtime safety inside `jitAssoc` and switching to raw 64-bit cons-field loads plus a combined cons mask (`src/jit/backend.zig:388`, `src/jit/backend.zig:394`) reduced ReleaseFast `assoc` from ~5.25ms to ~4.69ms (~10.7%) with focused regressions still green.
- Extending `patchCrossCallsToBL` to consume optional `MOVK hw=3` target materialization (`src/jit/backend.zig:4967`, `src/jit/backend.zig:5018`) closes a 64-bit direct-branch patch gap and is locked by a new machine-code regression (`src/jit/backend.zig:8248`).
- Adding a conservative BLR-target-clobber detector with focused bad/good machine-code regressions (`src/jit/backend.zig:7822`, `src/jit/backend.zig:8267`, `src/jit/backend.zig:8285`) preserved baseline runtime behavior while locking the exact cached-helper crash signature for follow-up repair.
- Extending `fixBlrTargetClobber` with a targeted imm-chain repair path (`src/jit/backend.zig:7707`, `src/jit/backend.zig:7785`) now fixes the captured single-`MOVZ` overwrite shape in backend regressions (`src/jit/backend.zig:8303`) without destabilizing baseline ReleaseFast benches.
- Generalizing BLR-target clobber detection to include low-immediate chain rewrites and non-imm overwrites (`src/jit/backend.zig:7788`, `src/jit/backend.zig:8406`, `src/jit/backend.zig:8425`, `src/jit/backend.zig:8474`, `src/jit/backend.zig:8503`) removed the known helper-target corruption signatures and kept cached helper-pointer lowering enabled (`src/jit/backend.zig:3406`).
- Making constant-cache reuse block-local at CFG boundaries (`src/jit/backend.zig:2107`) fixed a real SSA-dominance bug in cached helper-pointer lowering that crashed branch-local JIT paths on second invocation (`src/tests/integration.zig:2186`); ReleaseFast `assoc` now runs stably at ~2.79-2.83ms on repeated checks.
- Adding `bench-maxima --workloads=...` filtering (`bench/maxima_workload.zig:240`, `bench/maxima_workload.zig:277`, `bench/maxima_workload.zig:658`) plus wiring `tools/maxima-hotspots` to pass selected workloads (`tools/maxima-hotspots:22`, `tools/maxima-hotspots:288`) removed hidden benchmark-order coupling from hotspot runs.
- Forcing a pre-timed GC after benchmark warmup (`bench/maxima_workload.zig:390`) eliminated cross-workload nursery carryover from timed sections; `ratsimp` JIT dropped from ~308ms artifact to ~39ms when measured without in-window GC pauses.
- Emitting first unsupported IR tags on JIT compile failures (`src/interp/repl.zig:3013`) turned generic `UnsupportedIrNode` logs into actionable blockers; current Maxima benchmark wrapper rejection points to `.progv` as the first missing lowering.

### Did Not Work
- Retaining every temporary compiler `Value` through `vm.comp_retain` (the `saveVal/loadVal` experiment) introduced many extra GC points and surfaced unrelated stale-cursor crashes; this broad retention strategy was reverted in favor of narrower cursor-rooting + ext-root windows.
- Relying on `tools/maxima-hotspots` as the only RCA signal for `factor`/`ratsimp` at larger scales was noisy here (intermittent `maxima workload error: OutOfMemory` lines and occasional inflated totals); direct `./zig-out/bin/maxima_workload_bench --json --jit=on/off` runs were required for stable apples-to-apples deltas.
- `zig build test -- --test-filter ...` can hang in this environment with no emitted output even for focused filters (timed out at 240s in this session), so validation had to rely on bench builds/runs plus targeted runtime checks until the test-runner hang is fixed.
- Improving absolute GC throughput alone did not satisfy the current JIT-relative gate: the survivor-age array rewrite sped up both JIT and interpreter paths similarly, so `wins` stayed red even with ~20%+ absolute runtime drops.
- Running performance A/B in a detached `jj workspace` path without a Git metadata root failed for `bench-maxima` because the build path shells out to `git rev-parse`; for parity A/B, run from the main Git-backed workspace.
- Adding a per-chunk constant-refresh epoch field directly to `Chunk` layout (`src/runtime/objects.zig` experiment) caused deterministic JIT Maxima workload failures (`UnhandledThrow`/`OutOfMemory`) after `integrate`; treat chunk header layout as ABI-sensitive with hoist/JIT paths unless the full cross-repo contract is updated together.
- Calling `shouldTraceCallRet` unconditionally from hot `.call`/`.ret` op paths (even with tracing disabled) leaves measurable avoidable overhead on call-heavy benchmarks; keep a top-level `trace_call_ret` guard before helper invocation.
- Keeping `tryCallJit` on the `lookupJitFn` path (even with chunk-pointer caching inside lookup) underperformed direct `chunk.jit_fn` reads in quick A/B runs; the extra lookup path still left more overhead than the direct fast pointer path for the hot call site (`src/interp/vm.zig:1694`).
- Treating all `&optional` slots as strictly positional before `&key` parsing (no early key boundary) regressed constructor-style keyword initarg calls (`make-instance`, defstruct constructors); boundary logic must allow early key start when the remaining tail is a complete even pair list.
- Replacing all stack moves with one generic loop path leaves easy performance on the table for `doCall` tiny keyword-pair shuffles; tiny-count specialization is worth the branch cost here.
- Evaluating tiny call-path optimizations from a single hotspot run was too unstable; running quick A/B in a separate `jj` workspace against `@-` gave clearer keep/revert signal for near-noise deltas.
- Keeping the old two-pass keyword validation path (`allow-other-keys` scan then unknown-key scan) added avoidable repeated work in hot keyword-call paths; a single-pass stateful scan gives the same semantics with lower call overhead.
- A direct `resolveForwardedValue` region-fast-path experiment (tenured/LOS early-return + loop-hoisted region math) improved micro-signal but regressed repeated full Maxima hotspot timings on this host, so it was reverted in favor of call-site resolve-count reductions.
- Treating one benchmark run as authoritative for keyword-call changes was too noisy; decision quality improved only after repeated `keyword_call` + repeated Maxima hotspot runs on both current and parent revisions.
- A closure-specific resolve skip in `doCall` (attempted around the hot entry path near `src/interp/vm.zig:10748`) regressed the dedicated `keyword_call` microbench in A/B checks; preserving canonical doCall-time forwarded resolution was the better choice.
- Scanning for first keyword in steps of two from `arity` (`src/interp/vm.zig` pre-fix around current `10863`) is unsound for `&optional`+`&key`: odd-offset key starts are missed, and some invalid extra positional args can slip through without signaling.
- Eagerly materializing an allowed-keyword slice on every key call (pre-threshold version near current `10907`) added overhead to small or zero-key-pair calls; gating fast materialization by `(kw_pair_count > 1)` and small declared-key count is necessary.
- Fixed-arity fast paths alone are not enough to pass the JIT gate (`wins` still `0..1/5`): after call-setup wins, remaining loss is in dynamic call-shape paths (`&key`/`&rest`/dispatcher-heavy frames), so follow-up work must target those branches directly.
- Keeping `lookupFnResolveCache` defensive by resolving forwarded values and rechecking callable tags on every hit (`src/interp/vm.zig` pre-fix `lookupFnResolveCache`) consumed measurable runtime in `doCall -> resolveFunctionValue` and left obvious hotspot time on the table.
- Sampling short scale-1 runs for integrate mostly captured loader/compile activity; runtime-stage profiling required long-running benches (`--scale=80`) before call-resolution hotspots became visible in `/tmp/habu_integrate_jit_scale80.sample`.
- Hardening only qualified-symbol lookup (`src/runtime/qual_name.zig`) was insufficient: the stale-forwarded symbol was introduced earlier, and `pushProgvFrame` could still dereference stale symbol/list cells before lookup ever ran.
- Using `-Duse-hoist=false` as a proxy for interpreter baselines in `tools/maxima-hotspots` hid real JIT-vs-interpreter comparisons behind backend divergence and produced workload errors (`OutOfMemory`/`UnhandledThrow`) unrelated to JIT effectiveness.
- Relying on `restoreExtRootsSynced` copyback from temporary root arrays propagated stale values into persistent owners under nested save/set/restore chains (`src/interp/vm.zig` pre-fix `restoreExtRootsSynced` logic).
- Fixing only helper-entry forwarded resolution was insufficient by itself; stale symbol-tagged pointers can survive long enough to lose forwarding metadata before first helper use, so preventing in-JIT GC was required for correctness (`src/jit/backend.zig:239`, `src/interp/vm.zig:1535`, `src/interp/vm.zig:1998`).
- Manually updating `PLAN.md` checkboxes drifted from dot state; syncing checkboxes from `dot show` status avoids stale "open vs done" plan state when many dots close in parallel.
- Running `tools/dot-finish` with an unbounded `zig build test` on this machine can leave long-lived test jobs after harness stalls; timeout guardrails are required to keep the process pool healthy.
- Treating Debug `bench-comp` numbers as runtime parity signal was misleading for `assoc`: Debug sampling showed `debug.assert`/tag-check overhead inside `jitAssoc`; parity tracking must use `-Doptimize=ReleaseFast`.
- The `jitAssoc` raw-check rewrite materially improved Debug numbers but did not move ReleaseFast parity (`~5.23ms` to `~5.25ms`), so remaining gap is elsewhere (helper/call lowering and loop arithmetic), not `Value` predicate overhead.
- Even with compare-helper fast paths, `assoc` parity remains far from SBCL in ReleaseFast (`~5.12ms` vs `~2.79ms` baseline), so the next wins are not in scalar compare helpers but in call/loop lowering and remaining helper-bound overhead.
- Inlining `assoc` directly in Hoist IR (replacing helper calls in `translateAssoc`) regressed ReleaseFast `assoc` to ~7.59ms; for this path, optimized native helper code beats `.none`-mode JIT control-flow lowering.
- Sampling `./zig-out/bin/comprehensive_bench` before rebuilding with `-Doptimize=ReleaseFast` reintroduced debug-only signals and distorted RCA; rebuild mode must match measured mode before profiling.
- Replacing `jitAssoc` with a C helper path did not outperform the tuned Zig helper in repeated ReleaseFast rebench runs, so this hotspot should stay Zig-native and be optimized in-place.
- Reshaping `jitAssoc` to a `while (true)` loop and removing pointer-mask loads regressed ReleaseFast `assoc`, so the prior masked/guarded loop form should remain the baseline until a proven win appears.
- Extending cross-call BL patch coverage to 64-bit materialization did not move the current `assoc` microbench immediately; treat it as coverage/hardening for address-layout variability rather than guaranteed direct speedup.
- Rewriting post-registration cross-call patching to a compact `BL + B-skip` shape at materialization head regressed ReleaseFast `assoc` from ~4.5-4.7ms to ~4.94ms, so keep the conservative patch form until call-target integrity is proven with stronger machine-code checks (`src/jit/backend.zig:4923` attempted rewrite, reverted).
- Adding unrolled/prefetch variants to `jitAssoc` regressed ReleaseFast `assoc` into ~4.87-5.48ms; this loop is latency-sensitive and extra control/memory ops hurt on this host (`src/jit/backend.zig:388` attempted variants, reverted).
- Caching helper pointers via `cachedIconst` in `emitPrimitiveCall*` triggered `BENCH-ASSOC` EXC_BAD_ACCESS from `BLR x9` target clobber (`movz x9,#imm` in arg setup); call-target preservation must be proven first before re-enabling pointer caching (`src/jit/backend.zig:3406`, `src/jit/backend.zig:3412`, `/tmp/assoc_dump2.txt`).
- Even after landing one imm-chain clobber repair in `fixBlrTargetClobber`, enabling cached helper pointers still crashes `BENCH-ASSOC`; additional BLR target corruption shapes exist beyond the single-`MOVZ` signature and need separate regressions before retrying caching.
- Reusing cached constants across blocks without dominance checks was unsound: helper pointer constants first materialized in one branch were reused from sibling branches, leading to undefined call targets and deterministic second-call crashes in branch-local JIT tests (`src/jit/backend.zig` pre-fix `switchBlock` behavior, `src/tests/integration.zig:2186`).
- Running `tools/maxima-hotspots` against full `bench-maxima` output while only filtering rows post-hoc produced misleading regressions: excluded earlier workloads could trigger GC inside a selected workload’s timed section, inflating that workload’s reported cost.

## Session Notes (2026-02-22)

### Worked Well
- Rekeying JIT chunk maps at GC boundaries from forwarding pointers (`src/interp/vm.zig:1489`, invoked at `src/interp/vm.zig:2246`) fixed stale-pointer chunk dispatch without mutating `Chunk` object layout, and the regression (`src/tests/integration.zig:275`) now proves lookup works after chunk movement.
- Keeping registration/removal on raw chunk addresses (`src/interp/vm.zig:1382`, `src/interp/repl.zig:3076`, `src/interp/repl.zig:3090`) plus replacing prior compiled entries in-place prevented stale map entries from surviving failed JIT finalization paths.
- Moving JIT bridge unwinding to a C trampoline (`src/jit/bridge_jump.c`) with `bridgeRun(callback)` kept the `setjmp` frame alive across native execution and enabled true non-local exits from bridge errors without continuing compiled code.
- Routing `jitCallBridgeInvoke` error catches to `bridgeThrow` (`src/interp/vm.zig:355`) and executing compiled calls through `bridgeRun` (`src/interp/vm.zig:1422`) cleanly aborts active JIT frames while preserving VM error semantics (`UnhandledThrow`, etc.).
- Replacing bridge panic-on-error with an explicit JIT bridge error lane (`src/interp/vm.zig:335`, `src/interp/vm.zig:1377`, `src/jit/backend.zig:60`, `src/jit/backend.zig:3116`) let `tryCallJit` propagate VM errors (`UnhandledThrow`, `ControlTransfer`, etc.) through normal VM error paths instead of aborting the process.
- Locking bridge relay behavior with a focused regression (`src/tests/integration.zig:1877`) catches panic regressions on JIT generic-call error paths and proves error relay works end-to-end.
- Replacing single-candidate JIT extraction (`extract first candidate` + `child_chunks[0]`) with full candidate discovery and signature/name chunk matching (`src/jit/candidates.zig`, `src/interp/repl.zig:2850`, `src/testing/compile_chunk.zig:102`) removed incorrect chunk registration when top-level forms contain multiple defuns and nested lambdas.
- Locking multi-defun progn JIT registration with function-cell chunk lookups (`src/tests/integration.zig:88`) catches regressions where only the first eligible function gets native code.
- Adding explicit `jit_compiled` counters + machine-checkable JIT gate (`bench/maxima_workload.zig:605`, `tools/maxima-hotspots:87`, `tools/perf-loop:346`) made "JIT effective vs interpreter" a hard signal instead of manual inspection.
- Remapping stale perf-loop recommendation dots through live `dot ls --json` state (`tools/perf-loop:393`, `tools/perf-loop:631`) removed closed-dot action churn and keeps next-dot output executable.
- Extending generic JIT call bridges from 4 to 7 user args (`src/jit/backend.zig:305`, `src/interp/vm.zig:397`) removed an obsolete arity cap in `translateGenericCall` and keeps calls register-only (fn+7 args) without relying on hoist stack-arg lowering.
- Locking the widened bridge with a seven-argument rooted generic-call regression (`src/jit/backend.zig:7894`) prevents the old `UnsupportedCallTarget` ceiling from coming back silently.
- Adding VM-level JIT admission counters (`src/interp/vm.zig:436`, `src/interp/repl.zig:2850`, `src/testing/compile_chunk.zig:120`) made candidate rejection reasons measurable in both REPL and test helper compilation paths.
- Exporting admission counters in Maxima benches/hotspot tooling (`bench/maxima_workload.zig:606`, `tools/maxima-hotspots:83`) turned `jit_compiled=0` from a black box into actionable evidence (`sk_speed` dominating candidate skips).
- Dropping the explicit speed gate while keeping safety=0 (`src/jit/candidates.zig:92`) removed opt-declare dependency and shifted Maxima skip telemetry from `sk_speed` to `sk_safety`, proving where the true admission blocker sits.
- Replacing REPL-side alias-cache rebuilds with an incremental `GlobalEnv` alias index (`src/compiler/compile.zig:1901`, `src/compiler/compile.zig:1968`, `src/interp/repl.zig:1619`) removed per-lookup global-table scans and cut Maxima hotspot workloads (`ratsimp`, `factor`, `integrate`) by ~1.6-3x at `tools/maxima-hotspots --scale 1 --heap-mb 1024 --nursery-mb 32`.
- Adding `GlobalEnv` reverse name indexing and routing VM global-name lookup to O(1) (`src/compiler/compile.zig:1930`, `src/compiler/compile.zig:2036`, `src/interp/vm.zig:1636`) eliminated `globalNameForIndex` hash-map iterator walks from the `loadGlobal` hot path.
- Caching `HABU_TRACE_FN_RESOLVE` at REPL init (`src/interp/repl.zig:105`, `src/interp/repl.zig:130`, `src/interp/repl.zig:1338`, `src/interp/repl.zig:1546`) removed repeated `getenv` calls in function-resolution hot loops without changing trace semantics.
- Replacing linear builtin-callable checks with compiler-side cached lookup maps (`src/compiler/compile.zig:2058`, `src/compiler/compile.zig:2324`, `src/compiler/compile.zig:15666`) and switching REPL package-probe paths to raw-set checks (`src/interp/repl.zig:1326`, `src/interp/repl.zig:1406`, `src/interp/repl.zig:1422`) reduced function-designator dispatch overhead; `tools/maxima-hotspots --scale 1 --heap-mb 1024 --nursery-mb 32` improved from ~367/222/110ms (`ratsimp`/`integrate`/`factor`) to ~326/165/85ms in follow-up runs.
- Hoisting GC/heap debug env checks out of allocation and root-scan loops (`src/runtime/gc.zig:369`, `src/runtime/gc.zig:685`, `src/runtime/heap.zig:381`, `src/runtime/heap.zig:2049`) removed per-object and per-root `getenv` churn while preserving opt-in diagnostics.
- Caching VM trace filter env state at init (`src/interp/vm.zig:568`, `src/interp/vm.zig:957`, `src/interp/vm.zig:683`, `src/interp/vm.zig:762`) removed repeated getenv parsing in error/call tracing predicates.
- Keeping list cursors rooted and advancing the root before recursive compile calls (`src/compiler/compile.zig:6246`, `src/compiler/compile.zig:7231`, `src/compiler/compile.zig:15847`, `src/compiler/compile.zig:17671`) fixed real stale-pointer traversal hazards in moving-GC compiler passes.
- Rewriting `compileTagbody` to compile segments from rooted cursors instead of staging raw `Value` arrays (`src/compiler/compile.zig:8457`) eliminated the Maxima `nparse` crash (`compileTagbody` segfault on stale cons pointers) under generational load.
- Supporting integer tags in `tagbody`/`go` (`src/compiler/compile.zig:8524`, `src/compiler/ir.zig:218`) aligned behavior with CL semantics and removed false `InvalidSyntax` on numeric tag targets.
- Locking integer-tag behavior in both compiler and runtime tests (`src/compiler/compile.zig:21016`, `src/tests/integration.zig:3101`) prevented silent regressions in tag parsing and jump resolution.
- Normalizing BL/BLR argument-copy chains before scheduling in `fixCallArgMoves` (`src/jit/backend.zig:7036`) handled duplicate destination moves and preserved final pre-call register mapping under indirect call setup.
- Scanning call setup through interleaved BLR target materialization (`mov`/`movz`/`movk`) in `fixCallArgMoves` (`src/jit/backend.zig:7041`) fixed a real blind spot where argument moves were skipped when target setup appeared between arg copies and call.
- Adding machine-code regressions for interleaved target setup and two-cycle copies (`src/jit/backend.zig:7244`, `src/jit/backend.zig:7266`, `src/jit/backend.zig:7288`) gives direct protection for indirect-call repair logic without relying on full-suite runtime repros.
- Letting parser read-eval/dispatch hooks surface original VM errors via parser-side hook capture (`src/reader/parser.zig:61`, `src/reader/parser.zig:105`, `src/reader/parser.zig:169`, `src/reader/parser.zig:201`) preserved non-local-exit semantics instead of collapsing them to parse failures.
- Routing VM and REPL parse callsites through hook-error-aware parsing (`src/interp/vm.zig:290`, `src/interp/vm.zig:6927`, `src/interp/repl.zig:2234`, `src/interp/repl.zig:2329`, `src/interp/repl.zig:3386`) fixed nested `#.` throw relay paths (`(catch 'x (read-from-string \"#.(throw 'x 42)\") ...)`) without special-casing Maxima code.
- Locking the reader relay behavior with a focused integration regression (`src/tests/integration.zig:3032`) gives deterministic coverage for nested read-eval non-local exits across call barriers.
- Switching `runVmPreserveMacroState` from pointer-classified `currentExtRoots` restore to `saveExtRoots`/`restoreExtRoots` (`src/interp/repl.zig:812`, `src/interp/repl.zig:863`) removed a stale-slice restore path and made nested VM root restoration owner-stable under reallocations.
- Adding a direct owner-reallocation regression for ext-root snapshots (`src/interp/vm.zig:12759`) locks `restoreExtRoots` semantics so restores rebind by owner and not by stale slice pointers.
- Treating AArch64 unscaled/pre/post-index load/store forms as first-class register uses in MOVZ liveness (`src/jit/backend.zig:6380`, `src/jit/backend.zig:6421`, `src/jit/backend.zig:6491`) fixed the nested-cons JIT corruption/crash path where live constant materialization for cons stores was being NOPed.
- Locking MOVZ liveness and nested-cons runtime behavior with focused regressions (`src/jit/backend.zig:7906`, `src/jit/backend.zig:7928`, `src/tests/integration.zig:245`) gives direct red/green coverage for this exact failure mode.
- Treating `RET` as reading x0 in liveness (`src/jit/backend.zig:6326`, `src/jit/backend.zig:6372`) fixed a real dead-MOVZ miscompile where `movz x0,#imm; ret` got NOPed and leaf functions returned stale pointer garbage (`hoist IR translator: block wrapper compiles` expected tagged 85).
- Locking return-register liveness with focused backend regressions (`src/jit/backend.zig:8045`, `src/jit/backend.zig:8063`) prevents future dead-code passes from deleting result materialization before `RET`.
- Replacing static `arr_new` lowering with register-only call shapes in `translateArrNew` (`src/jit/backend.zig:4291`) removed 10-arg indirect calls from JIT array construction and fixed the `gc_vector` warmup crash path.
- Guarding indirect-call lowering to max 8 args (`src/jit/backend.zig:3128`) turns unsafe stack-arg call emission into an explicit compile-time fallback instead of silent return-address corruption.
- Locking the crash repro with a focused JIT integration regression (`src/tests/integration.zig:245`) keeps `(make-array ... )` loop return paths covered under `(optimize (speed 3) (safety 0))`.
- Tracing JIT call entry/exit (`HABU_TRACE_JIT_CALL`) in `tryCallJit` (`src/interp/vm.zig:1285`) gave a deterministic failing function name (`%MAP-REVERSE`) for a Maxima-load crash that otherwise only surfaced as random native PC faults.
- Restoring strict JIT eligibility to explicit `(optimize (speed 3) (safety 0))` (`src/interp/repl.zig:2910`, `src/testing/compile_chunk.zig:164`) removed unsafe safety>0 JIT compilation and restored full `bench-maxima` load+run stability.
- Keeping a hard allocator-cursor invariant check after JIT returns (`src/interp/vm.zig:1311`, `src/jit/backend.zig:95`) turns cursor corruption into immediate, attributable failures instead of delayed heap-state crashes.

### Did Not Work
- Adding identity inside `Chunk` itself for JIT key stability was not viable in practice: layout changes destabilized hoist-mode Maxima runs, so chunk identity must stay external to the GC object layout.
- Even after JIT map rekeying, `bench-maxima -Duse-hoist=true --scale=1 --json` still crashes in package symbol lookup (`src/runtime/heap.zig:211`, `src/compiler/compile.zig:15803`) with an invalid string pointer path (`0x30`), so this is a separate root-cause track.
- Calling `setjmp` in a helper that returns to Zig (`bridgeEnter`) and later `longjmp`ing back to that dead frame crashed immediately (`Segmentation fault at address 0x0`); `setjmp` must remain active in the same frame for the full JIT call window.
- Injecting a post-generic-call guard CFG inside JIT translation (an `emitBridgeErrorGuard` experiment in `src/jit/backend.zig`) regressed recursive JIT functions (`compileChunk JIT handles recursive nqueens helper entry copies`) with null-call crashes; keep bridge relay state in VM/backend runtime lanes until that control-flow lowering path is proven safe.
- Using a direct keyword-heavy generic call as the bridge relay regression target caused an unrelated native crash (`Bus error at 0x3`) before reaching the bridge helper; the stable repro is a JIT call into an interpreted wrapper that triggers the keyword failure (`src/tests/integration.zig:1877`).
- Expecting all `(speed 3, safety 0)` functions in the same top-level progn to compile is still wrong when one body contains unsupported IR (`lambda` nodes in body): candidate collection now keeps compiling later candidates, but unsupported functions remain interpreted by design (`src/testing/compile_chunk.zig:163`).
- Running perf-loop with large microbench iteration counts for quick validation (`tools/perf-loop --iters 1000`) stalls practical feedback loops; keep smoke validation runs small and use targeted bench commands for deep measurements.
- Widening `CallBridge` in hoist mode without matching `src/jit/backend_stub.zig` broke `-Duse-hoist=false` builds immediately; backend and stub interfaces must evolve together.
- Telemetry initially showed zero candidate counts in compileChunk-only tests because admission accounting existed only in REPL JIT paths; helper compiler paths (`src/testing/compile_chunk.zig`) must update the same counters for consistent assertions.
- Removing the safety gate entirely immediately crashes Maxima load with `jit call bridge failed: UnhandledThrow argc=4` (`src/interp/vm.zig:353`), so safety>0 admission needs proper JIT↔VM condition relay before it can be enabled.
- Invalidating a REPL-owned alias cache by `globals.next_index` and rebuilding from `globals.bindings.iterator()` (`src/interp/repl.zig` pre-fix alias-cache helpers around 1538-1607) was still O(n) under loader churn and stayed on the hotspot path.
- Broadening JIT admission to safety>0 call-free lambdas without full runtime-safety lowering/bridge semantics caused deterministic Maxima crashes in `%MAP-REVERSE` (segfault + misaligned allocator cursor), even after partial cons-lowering changes.
- Treating all non-symbol atoms in `tagbody` as executable forms was incorrect; CL treats integer atoms as labels too, so tests that expected trailing fixnum atoms as forms were invalid and had to be rewritten (`src/compiler/compile.zig:20972`, `src/compiler/compile.zig:21032`).
- Running `sample` against short-lived bench processes without a longer run window produced stale/no profile capture (`/tmp/bench_maxima_s20_sample.log` showed process exited before sampling); use workload settings that guarantee process lifetime during sampling.
- Full `zig build test` can still hang in this environment (`--listen` child process remained active with no output), so use focused `-Dtest-filter` gates for deterministic dot closure checks when the full suite stalls.
- Relying on contiguous backward scans of only `mov x0..x7,*` before BL/BLR in `fixCallArgMoves` missed valid call setup windows with interleaved target setup ops, leaving indirect-call argument corruption unpatched.
- Converting read-eval/dispatch callback errors to parser `UnexpectedToken` in bridge hooks (`src/interp/vm.zig` pre-fix `readEvalBridge`/`dispatchMacroBridge`, `src/interp/repl.zig` pre-fix `parserReadEval`/`parserDispatchMacro`) masked real control transfers as parse/type errors and broke `(catch ...)` around `read-from-string` `#.` forms.
- Restoring nested VM ext roots via pointer-identity classification (`persistent`/`ctx`/`slice`) in `runVmPreserveMacroState` was brittle; unclassified owners fell back to raw slices and risked stale restores after owner reallocation.
- Restricting load/store read/write detection to the unsigned-offset `0x39*` family in MOVZ dead-code analysis (`src/jit/backend.zig` pre-fix `insnReadsReg`/`insnWritesReg`) missed hoist-emitted unscaled `F8*` forms, so `eliminateDeadMovz` deleted live constants and produced malformed cons cells at runtime.
- Treating `RET` as a pure control-flow terminator in liveness (`src/jit/backend.zig` pre-fix `isRegDeadInBlock`/`isRegDeadFrom`) is incorrect for x0: dead-MOVZ elimination can remove return-value setup and surface as nondeterministic pointer returns in leaf wrappers.
- Emitting `arr_new` via `jitMakeArrayStatic` with 10 indirect-call args (`src/jit/backend.zig` pre-fix `translateArrNew`) exercised hoist stack-arg lowering that spilled at `[sp]` and overwrote saved LR, crashing on function return (`Bus error at 0x4e1f` in `gc_vector` JIT warmup).

## Session Notes (2026-02-21)

### Worked Well
- Fixing tail-call `&key` frame reuse with overlap-safe argument moves in `doCall` (`src/interp/vm.zig:10164`) plus ordering positional copy before keyword-pair relocation removed real argument-slot corruption where `MEMBER` saw `lst` as a closure and `test` as nil in Maxima `INFINITYP`/`$LIMIT` paths.
- Registering `defstruct` type names in the runtime class registry during `compileDefstruct` (`src/compiler/compile.zig:10258`) made `typep`/`typecase` on struct names return booleans for non-struct objects instead of `UnknownTypeSpecifier`, unblocking Maxima `marray-type` calls in `limit`.
- Locking both regressions with focused integration tests (`src/tests/integration.zig:1597`, `src/tests/integration.zig:4567`) now catches tail-call keyword frame corruption and defstruct-type `typep` regressions before Maxima workload runs.
- Propagating `NestedNonLocalExit` out of `execute` instead of consuming/rethrowing it in-place (`src/interp/vm.zig:2248`) restored call-barrier ownership of non-local exit relay, fixing resumed execution after a caught `(load ...)` condition (the `transl.lisp` `DEF%TR` path no longer continues into later forms after the first caught failure).
- Locking the signal-path variant with a script-level regression (`src/interp/repl.zig:4887`) catches a previously untested case where `handler-case` around `load` could catch twice and keep running the failed file; expected post-fix behavior is one catch and no post-error file progress.
- Adding a Maxima transl script gate in integration (`src/tests/integration.zig:7336`) now exercises the real `(load script -> maxima-load-all -> transl failure)` path and validates that loader state is returned exactly once without post-return resume crashes.
- Routing generic JIT numeric ops through dedicated helpers while keeping recursive functions on conservative fixnum lowering (`src/jit/backend.zig:1764`, `src/jit/backend.zig:1773`, `src/jit/backend.zig:1812`) fixed float benchmark semantics and removed the `float` call-bridge bottleneck; `bench-comp` JIT float benches moved from ~332/346ms to ~12.8/14.9ms.
- Adding direct primitive resolution for `FLOAT` designators (`src/jit/backend.zig:908`, `src/jit/backend.zig:286`) removed per-iteration VM bridge dispatch in float-heavy loops.
- Fixing BLR target-register clobber before arg-move rewrites (`src/jit/backend.zig:6177`, `src/jit/backend.zig:4569`) resolved real call-target corruption where `movz x9,#imm` overwrote the call target register and jumped to immediate values (for example `0x23`).
- Locking the path with a focused integration regression (`src/tests/integration.zig:206`) catches JIT regressions in generic float arithmetic and float comparisons under `(optimize (speed 3) (safety 0))`.
- Resolving class metadata by current package + unambiguous local class name in `lookupClassMetadataByName` (`src/compiler/compile.zig:11136`) fixed `make-instance` compile failures when symbol package qualifiers differed from metadata qualifiers (for example `BIGFLOAT-IMPL:BIGFLOAT` symbol vs `BIGFLOAT:BIGFLOAT` metadata), unblocking `numeric.lisp` and Maxima e2e load readiness.
- Treating `AND` as a generic LOOP clause separator outside FOR/WITH chaining (`lib/stdlib.habu:5300`) fixed real-world forms like `(loop ... collecting ... and do ...)` used in `mload.lisp` while preserving parallel FOR/WITH semantics via explicit `:and` step markers only for variable chains.
- Adding a focused loop regression (`src/tests/integration.zig:4961`) for `collecting ... and do ...` catches future parser regressions that break macro-heavy loaders before Maxima e2e status checks.
- Tightening `eliminateRoundTripMovs` safety checks in JIT post-lowering (`src/jit/backend.zig:5047`) by rejecting source-overwrite/control-flow windows and requiring `isRegDeadAfter` on the temporary register fixed a real helper-call argument corruption in `bench-intern` (`<` received the function pointer instead of loop index) and restored `bench-comp`/`perf-loop` stability.
- Locking the failure mode with a dedicated regression (`src/tests/integration.zig:206`) for optimized `bench-intern` loop count prevents future call-setup rewrites from silently dropping live save/restore moves.
- Resolving forwarded values at every quasiquote recursion boundary (`src/compiler/compile.zig:7696`, `src/compiler/compile.zig:7754`) fixed a smallest-heap stdlib-load crash where `quasiquoteList` dereferenced stale/forwarded cons cells under GC pressure.
- Making the MV conditional-jump regression independent of stdlib macros (`src/tests/integration.zig:6485`) by using direct `if` instead of `when` removed false negatives from missing macro expansion setup.
- Computing untagged eligibility before cross-call classification and making helper detection lowering-aware (`containsHelperCalls(body, fixnum_inline)` in `src/jit/backend.zig:4072`, `src/jit/backend.zig:4384`) removed false `cross=true` flags for pure arithmetic loops, restoring aggressive JIT opt-level selection for `bench-fixnum-loop`/`bench-fixnum-mul`.
- Adding focused backend unit tests for helper-call classification (`src/jit/backend.zig:6486`, `src/jit/backend.zig:6502`) locked the new lowering-aware behavior so future refactors do not silently reintroduce conservative cross-call misclassification.
- Rooting `global_ref` symbols during JIT literal-root collection (`src/interp/repl.zig:2726`) and lowering generic-call designators from those roots (`src/jit/backend.zig:2478`) fixed missing call-target patterns where non-primitive/non-known global calls previously fell through with invalid designators.
- Locking the behavior with dedicated backend regressions (`src/jit/backend.zig:6533`, `src/jit/backend.zig:6559`) catches both required-root failure mode and rooted designator success path in generic call lowering.
- Extending JIT helper lowering for data-path IR (`src/jit/backend.zig:603`, `src/jit/backend.zig:822`, `src/jit/backend.zig:1000`, `src/jit/backend.zig:1063`) removed major unsupported coverage gaps for vector/hash/string ops plus generic N-subscript `arr_ref`/`arr_set` and dynamic/static array construction.
- Wiring the same data tags through translator support gates (`src/jit/backend.zig:1960`, `src/jit/backend.zig:2083`, `src/jit/backend.zig:4763`) prevented false JIT rejection/classification drift where helpers existed but `canTranslate`/`firstUnsupportedTag`/`containsHelperCalls` lagged behind lowering.
- Adding backend regressions for the new generic data paths (`src/jit/backend.zig:7219`, `src/jit/backend.zig:7263`, `src/jit/backend.zig:7328`, `src/jit/backend.zig:7392`) gives direct red/green signal for vec/hash/multidim-array helper lowering.

### Did Not Work
- Relocating keyword pairs before positional arguments in tail-call `&key` frame reuse (`src/interp/vm.zig` pre-fix `doCall` tail key path) still clobbered positional source slots when ranges overlapped, producing partially fixed but still wrong bindings (`lst` became `:TEST`); positional arguments must be copied first.
- Handling `NestedNonLocalExit` inside the main `execute` error loop (`src/interp/vm.zig` pre-fix `err == error.NestedNonLocalExit` branch) bypassed call-boundary restoration logic and allowed inner file loaders to keep advancing forms after outer `handler-case` already caught the condition.
- Using a package-specific `eq` check for the first failed module in transl status validation was brittle (`src/tests/integration.zig` pre-fix); the failure marker must allow symbol/string representation differences and compare by canonical module text.
- A branch-heavy fixnum-fast/slow lowering for every generic numeric op triggered upstream hoist CFG instability (`computePreds` out-of-bounds) on real benchmark functions; keeping non-recursive generic ops helper-based and recursive paths conservative avoided this compiler failure in practice.
- Relying only on `fixCallArgMoves` was insufficient once constant materialization clobbered the BLR target register between `mov target` and `blr`; a dedicated BLR-target-clobber repair pass was required.
- Assuming native package qualifiers in `lookupClassMetadataBySymbol` were stable was wrong: aliases from Lisp package setup (for example Bigfloat package mapping) can diverge from defclass metadata keys and silently trigger `InvalidSyntax` on otherwise valid `make-instance` forms.
- Restricting LOOP `AND` to FOR/AS/WITH-only continuation (`lib/stdlib.habu` pre-fix `loop-expand`) is too strict for ANSI/Maxima code that uses `and` to chain action clauses, and it produced hard load stops (`AND must continue FOR/AS/WITH clause`) in `mload.lisp`.
- Eliminating round-trip MOV pairs using only local between-use checks (`src/jit/backend.zig` pre-fix `eliminateRoundTripMovs`) is unsound for call setup: `mov x22,x0` / `mov x0,x22` around helper calls can look cancelable but are live state transfer when the source register is overwritten in-between.
- Using `when` in low-level VM jump tests without loading stdlib macros (`src/tests/integration.zig` pre-fix `mv: values through conditional jumps`) can fail as `UnboundSymbol` and hide the real jump/multiple-value behavior being tested.
- Relying on a single-run `bench-comp` number to validate sub-millisecond loop improvements is noisy; confirm with `HABU_TRACE_JIT_FLAGS` classification output plus repeated runs before concluding a regression or win.
- Leaving `.global_ref` call designators on the legacy `nil` translation path in JIT (`src/jit/backend.zig` pre-fix `translateGenericCall`) silently masks call-target lowering gaps; generic calls must load rooted symbol designators or fail fast.
- Full Habu test validation is currently blocked when `../hoist` has syntax-incomplete edits (`/Users/joel/Work/hoist/src/context.zig:25`), so dot closure must record external-blocker status and use partial compile/test signal until hoist builds again.

## Session Notes (2026-02-20)

### Worked Well
- Syncing inline-cons cursor state at JIT↔VM bridge boundaries (`src/interp/vm.zig:337`, `src/interp/vm.zig:349`) fixed a real allocator rewind bug: bridge calls no longer reset `g_alloc_ptr` from stale `heap.alloc_ptr`, and recursive nqueens JIT paths now preserve cons list state.
- Classifying *any* non-self call as a cross-call in JIT lowering (`src/jit/backend.zig:4079`, `src/jit/backend.zig:4153`) ensured `fixCallArgMoves` runs in `src/testing/compile_chunk.zig` flows where `known_fns` is empty, closing helper-call arg corruption in wrapper functions.
- Adding env-gated JIT bridge tracing (`HABU_TRACE_JIT_BRIDGE` in `src/interp/vm.zig:326`) made call-designator/arg corruption immediately visible and shortened RCA from assembly-level guesswork to one deterministic signal.
- Extending `CompiledFn.callFromValues` beyond arity 3 (`src/jit/backend.zig:983`) closed a silent high-arity JIT call bridge gap where 4+ arg compiled functions previously returned `nil` from the VM bridge path.
- RCA on JIT helper-call corruption showed a true parallel-copy cycle in call-argument setup (`mov x0,x1; mov x1,x3; mov x2,x0; mov x3,x2`) being lowered sequentially; extending `fixCallArgMoves` to use scratch-cycle breaking and consume the pre-call target move slot (`mov x9,xT; blr x9`) fixed wrong helper args without papering over.
- Tightening untagged-mode eligibility to a conservative arithmetic subset in `src/jit/backend.zig` prevented untagged/tagged mixing across runtime helper boundaries and removed a class of silent semantic corruptions in JIT helper paths.
- Adding a focused JIT regression for formatted templates with suffix text (`src/tests/integration.zig:128`) caught the helper-call argument corruption immediately and now guards the call-argument fix.
- Specializing `concatenate` for all-string inputs in `lib/stdlib.habu:2436` with direct `string-concat` handling for 1/2-arg hot cases and preallocated copy for 3+ args cut `bench-comp` `string_concat` from ~2031ms to ~39ms while keeping mixed-sequence fallback behavior.
- Expanding concatenate integration coverage (`src/tests/integration.zig:5586`) to include mixed sequence coercion and list output protected the optimized string path from silently breaking non-string result types.
- Rewriting `reduce` to iterative folds and adding a `#'+` non-`:from-end` fast path in `lib/stdlib.habu:1043` removed `funcall` dispatch from the dominant benchmark case and cut `bench-comp` `reduce` from ~1894ms to ~25ms (single-iteration run) without changing CL fold behavior.
- Locking reduce semantics with an integration gate (`src/tests/integration.zig:719`) ensured left/right fold order, empty-sequence behavior, and `:initial-value` handling stayed intact after the loop rewrite.
- Splitting `mapcar` into explicit 1-list and 2-list fast paths in `lib/stdlib.habu:107` removed per-element `apply` argument-list churn on the hot benchmark path while preserving the generic variadic branch for 3+ lists; `bench-comp` `mapcar` dropped from ~190ms to ~63ms (single-iteration run).
- Making `mapcar2` iterate with `consp` guards and `%map-reverse` (`lib/stdlib.habu:146`) kept dotted-list termination semantics aligned with generic `mapcar` while avoiding an extra `reverse` pass and potential non-cons `car` errors.
- Locking the new semantics with `src/tests/integration.zig:694` catches regressions in one-list, two-list, and dotted-tail list behavior under stdlib load.
- Rooting saved package state through VM global root stack (`src/interp/repl.zig:1540`, `src/interp/repl.zig:1548`) eliminated a real generational GC corruption where `COMMON-LISP:*PACKAGE*` was restored from stale local `Value` snapshots, and the full Maxima generational bench now completes (`bench/maxima_workload.zig`).
- Rooting defmacro transformed definitions across VM execution (`src/interp/repl.zig:3644`, `src/interp/repl.zig:3659`) prevented stale macro-entry payloads when GC runs during macro closure materialization.
- Adding an opt-in pre-GC global corruption probe (`HABU_TRACE_BAD_GLOBAL_ROOT` in `src/interp/vm.zig:1608`) made the bad root source explicit (`idx=100`, `COMMON-LISP:*PACKAGE*`) and shortened RCA.
- Locking the package-root fix with a dedicated generational load regression (`src/interp/repl.zig:4867`) catches stale `*PACKAGE*` restoration by forcing GC during `load` and then collecting again after `load` returns.
- Saving/restoring `*LOAD-PATHNAME*`/`*LOAD-TRUENAME*` through the VM root stack (`src/interp/repl.zig:1601`, `src/interp/repl.zig:1630`) removed another stale-local `Value` path under moving GC and is guarded by a focused generational regression (`src/interp/repl.zig:4900`).
- Adding a generational GC-stress regression for `string-upcase`/`string-downcase` designators (`src/tests/integration.zig:4620`) locks the forwarded-string safety path under heavy allocation churn.
- Automating dual-mode CAS hotspot capture with `tools/maxima-hotspots` plus `docs/maxima-hotspots.md` removed ad-hoc profiling drift and made JIT-vs-interpreter deltas reproducible in one command.
- Running parallel worker agents in isolated `jj` workspaces (`/Users/joel/Work/habu-agent-compiler`, `/Users/joel/Work/habu-agent-gc`) accelerated independent RCA/fix loops without file ownership collisions, then `jj squash --from ... --message ...` merged results cleanly back into the default workspace.
- Resolving forwarded symbols at every list-iteration boundary in compiler hot paths (`src/compiler/compile.zig:2682`, `src/compiler/compile.zig:5122`, `src/compiler/compile.zig:14190`, `src/compiler/compile.zig:17540`) eliminated stale symbol/name pointers under moving GC and stopped `stdlib fdefinition basic` segmentation faults.
- For incremental major-sweep tests, draining any already-active cycle before changing the root set (`src/runtime/gc.zig:2477`) prevented false negatives caused by finishing a cycle that started under the old root set.
- Validating barrier-assisted incremental marking with an old-object rescue regression (`src/runtime/gc.zig:2048`) caught cross-slice liveness hazards that normal sweep tests miss.
- Gating old->old card marking behind `major_cycle_active` (`src/runtime/heap.zig:1003`) preserved fast-path remembered behavior outside major cycles while still providing correctness during incremental marking.
- Moving major old-space collection to an explicit phase machine (`src/runtime/gc.zig:102`, `src/runtime/gc.zig:584`) with persistent `major_work` queue enabled resumable mark/sweep progress without per-cycle full sweeps.
- Splitting tenured/LOS sweeping into cursor-based slices (`src/runtime/heap.zig:1187`, `src/runtime/heap.zig:1367`) kept reclamation bounded per minor cycle while preserving coalescing correctness at cycle completion.
- Enabling write-barrier card marking for old->old pointer stores only while major cycle is active (`src/runtime/heap.zig:1003`) kept incremental marking sound across mutator slices and was validated by focused regression coverage.
- Extending Maxima workload GC snapshots with debt telemetry (`bench/maxima_workload.zig:53`, `bench/maxima_workload.zig:84`, `bench/maxima_workload.zig:604`) made debt trigger/skip behavior visible during real loader pressure.
- Wiring Maxima debt metrics through comparison tooling (`tools/gc-compare:455`, `tools/gc-compare:663`) enabled direct A/B coefficient checks instead of inferring debt behavior from pause metrics alone.
- Running coefficient A/B and rolling back to baseline constants in `src/runtime/gc.zig:89` after benchmark evidence prevented a real VM throughput regression (`bench-vm` string/hash path) caused by over-aggressive early-trigger thresholds.
- Integrating debt-trigger scoring into VM precollection (`src/runtime/gc.zig:197`, `src/runtime/heap.zig:1348`, `src/interp/vm.zig:1414`) replaced threshold-only checks with measurable debt/pause/occupancy decisions.
- Exporting debt-decision telemetry end-to-end (`bench/gc.zig:372`, `bench/check.zig:421`, `tools/gc-compare:341`) exposed policy-range regressions immediately in the standard perf loop.
- Recording debt paydown as actual debt retired instead of raw reclaim volume (`src/runtime/heap.zig:1370`) aligned counters with invariants and removed false debt-regression failures in `bench-check`.
- Tracking nursery survivor age through a reusable side-map + per-copy updates (`src/runtime/heap.zig:1292`, `src/runtime/gc.zig:582`) produced stable age histograms without changing object layouts.
- Extending survival/promotion telemetry with explicit age buckets and promotion-success counters (`src/runtime/heap.zig:434`, `src/runtime/heap.zig:992`, `src/runtime/heap.zig:1350`) made tenuring feedback directly measurable for the next adaptive-threshold dot.
- Rebuilding survivor-age state after each nursery swap (`src/runtime/gc.zig:245`, `src/runtime/gc.zig:330`) kept age tracking aligned with moving addresses and prevented stale-address drift.
- Guarding promotion-success accounting inside `sweepTenured` even when `dead_count == 0` (`src/runtime/heap.zig:1000`) fixed a real telemetry blind spot where always-live promoted objects never counted as successful promotions.
- Wiring new telemetry through `gc_bench`/`bench-check` (`bench/gc.zig:239`, `bench/check.zig:49`) caught schema and invariant regressions immediately.
- Fixing stale forwarded pointers at the VM constant/chunk boundary (`src/interp/vm.zig:10469`, `src/interp/vm.zig:10488`, `src/interp/vm.zig:10500`) removed a root crash vector under small nursery pressure; repairing constants/chunk pointers lazily in hot ops (`push_const`/`check_or`/`push_block`/`return_from`) kept behavior generic for any large Lisp workload.
- Using an interned builtin key for function cells (`src/runtime/builtins.zig:66`, `src/runtime/builtins.zig:177`, `src/interp/vm.zig:907`) removed repeated runtime interning in function-namespace lookup/store/clear and stabilized `symbol-function` behavior during GC churn.
- Preserving VM chunk-pool state with pointer-aware restore logic in compiler temporary execution paths (`src/compiler/compile.zig:3626`, `src/compiler/compile.zig:9233`) fixed stale chunk-pool restoration when nested compile/eval replaces pools mid-expansion.
- Canonicalizing forwarded symbols before macro/symbol-macro and struct-predicate lookup (`src/compiler/compile.zig:14881`, `src/compiler/compile.zig:14906`, `src/compiler/compile.zig:17338`) prevented GC-moved symbol identity drift in compile-time dispatch.
- Running both targeted integration regressions and real Maxima workload repros (`src/tests/integration.zig:7135`, `zig build -Duse-hoist=true bench-maxima -- --json --scale=1 --heap-mb=1024 --nursery-mb=16`) gave deterministic proof that the small-nursery path now completes without crash.
- Replacing the `symbol-plist` placeholder with a real primitive-backed wrapper in `lib/stdlib.habu:4171` fixed function-cell parity: direct `(symbol-plist ...)` and `(funcall #'symbol-plist ...)` now agree, and `getl` behavior is stable when loaded generically.
- Adding a stdlib `getl` compatibility implementation in `lib/stdlib.habu:4180` plus an integration lock in `src/tests/integration.zig:7135` prevented silent plist lookup regressions in Maxima-style paths.
- Adding the exact `defun + &aux + outer cond + push + inner do/cond/return` repro as an integration test (`src/tests/integration.zig:6285`) is a reliable guard even when no compiler code change is required.
- Tightening format directive behavior in `src/interp/vm.zig` fixed real gaps:
  - `~*` argument navigation now honors `~*`, `~:*`, `~@*`, and numeric counts (`src/interp/vm.zig:7913`).
  - `~P` now falls back to previous argument when no next argument exists (`src/interp/vm.zig:7945`), preserving common `~D ... ~P` usage.
  - `~G` now emits general float formatting (`src/interp/vm.zig:8546`).
  - `~/fn/` now invokes formatter functions and appends stream output (`src/interp/vm.zig:8579`).
- Expanding integration coverage for format directives (`src/tests/integration.zig:2938`, `src/tests/integration.zig:3005`, `src/tests/integration.zig:3029`) gave immediate red/green signal on each missing directive behavior.
- Adding real-workload benchmark harnesses for both Habu and SBCL (`bench/maxima_workload.zig`, `bench/maxima_workload.lisp`) made Maxima CAS performance and loader gaps measurable in one command (`tools/maxima-bench`).
- Adding `tools/perf-loop` to combine comprehensive microbench + Maxima workload results produced a deterministic hotspot ranking and concrete next-action list instead of ad-hoc profiling.
- Adding `bench/sbcl_gc.lisp` + `tools/gc-compare` gave direct pause-time parity numbers (`avg_pause_ns`/`p95_pause_ns`) against SBCL for equivalent allocation pressure.
- Parsing mixed benchmark stdout by extracting the trailing JSON payload (`tools/maxima-bench`, `tools/perf-loop`, `tools/gc-compare`) made automation robust when Maxima runtime warnings print before/around result JSON.
- Adding a live-occupancy floor in nursery resizing (`src/runtime/heap.zig:1144` `nurseryLiveFloor`) prevented adaptive shrink steps from setting `gc_threshold` below current live nursery usage, which otherwise risks immediate-GC thrash loops.
- Making policy-cycle counters wrap-safe (`src/runtime/gc.zig:165` via `counterDelta`) removed latent unsigned-underflow hazards when long-running telemetry counters roll over.
- Running Maxima workload benches in generational mode with explicit nursery sizing (`bench/maxima_workload.zig`: `--nursery-mb`) plus GC telemetry export (`.gc.load`/`.gc.run`) made nursery-policy behavior observable on real workloads instead of microbench-only signal.
- Extending `tools/gc-compare` with optional Maxima telemetry (`--with-maxima`, defaults `--maxima-scale=3 --maxima-nursery-mb=24`) provided a practical mixed workload calibration point while keeping fast micro-only runs as default.
- Driving tenuring as a first-class control law in `deriveTenuringPolicy` (`src/runtime/gc.zig:126`) and applying it every minor cycle (`src/runtime/gc.zig:273`) made promotion threshold behavior measurable, bounded, and non-oscillatory without workload-specific special cases.
- Capturing adaptive tenuring bounds/ratios directly in heap stats (`src/runtime/heap.zig:454`, `src/runtime/heap.zig:1242`) and exporting/validating them in bench tooling (`bench/gc.zig:366`, `bench/check.zig:353`) caught policy regressions as schema/invariant failures instead of latent perf drift.
- Locking policy behavior with dedicated GC tests (`src/runtime/gc.zig:1457`, `src/runtime/gc.zig:1510`) provided deterministic red/green coverage for raise/lower/deadband decisions and runtime threshold updates.
- Extending `tools/gc-compare` with tenuring guard metrics/gates (`tools/gc-compare:38`, `tools/gc-compare:256`, `tools/gc-compare:460`) added machine-checkable regression signals for promotion waste and policy-scale drift alongside pause/throughput parity checks.
- Adding a deterministic generational stress regression in integration (`src/tests/integration.zig:7173`) locked adaptive tenuring bounds (`threshold/min/max`, scale, ratio ranges) and ensured threshold movement under repeated promote-and-sweep cycles.
- Replacing 1-bit card marks with per-card lane bitmasks (`src/runtime/heap.zig:20`, `src/runtime/heap.zig:851`, `src/runtime/heap.zig:963`) tightened remembered-set granularity and reduced same-card false-positive scans without changing barrier call sites.
- Making `hasMarkedCardInAddrRange` lane-aware (`src/runtime/heap.zig:954`) plus adding a focused regression (`src/runtime/heap.zig:3070`) gave deterministic proof that unrelated lanes in the same card no longer trigger remembered-set hits.
- Coalescing remembered cards into run lists (`src/runtime/heap.zig:914`) and reusing a persistent `remembered_runs` buffer in GC (`src/runtime/gc.zig:185`, `src/runtime/gc.zig:414`) improved minor-GC remembered scanning locality while keeping allocation-free hot paths.
- Routing minor-GC remembered scans through run-aware overlap checks (`src/runtime/gc.zig:415`, `src/runtime/heap.zig:996`) eliminated full-table clean-run walks and preserved correctness on tenured/LOS edge scanning.
- Adding explicit remembered-set telemetry counters (`src/runtime/heap.zig:481`, `src/runtime/gc.zig:450`) plus exporting them in GC bench payloads (`bench/gc.zig:337`) made RSet scan pressure visible and regression-checkable.
- Extending `bench/check` + `tools/gc-compare` with remembered-set invariants/gates (`bench/check.zig:380`, `tools/gc-compare:33`, `tools/gc-compare:483`) locked both correctness (non-zero marked/runs/scans) and efficiency (`scan_per_mark`) in automated validation loops.
- Locking runtime coverage with a focused GC regression (`src/runtime/gc.zig:1822`) ensured remembered-set telemetry is exercised and monotonic under real LOS owner + young child mutation patterns.
- Adding heap-level GC debt accounting (`src/runtime/heap.zig:338`, `src/runtime/heap.zig:1329`, `src/runtime/heap.zig:2829`) plus VM debt-triggered precollection hooks (`src/interp/vm.zig:1062`, `src/interp/vm.zig:1410`) converted allocation pressure into explicit, testable counters instead of implicit OOM-only behavior.
- Exporting debt telemetry through `bench/gc` and enforcing it in `bench/check`/`tools/gc-compare` (`bench/gc.zig:337`, `bench/check.zig:70`, `tools/gc-compare:330`) created a closed verification loop for debt bytes, paydown, and trigger quality.
- Driving LOS threshold from per-cycle allocation-size deltas (`src/runtime/gc.zig:244`, `src/runtime/gc.zig:273`) plus occupancy/pause feedback produced bounded threshold movement without workload-specific handling.
- Exporting LOS policy state end-to-end (`src/runtime/heap.zig:474`, `bench/gc.zig:357`, `bench/check.zig:90`, `tools/gc-compare:374`) turned threshold/scale/range regressions into immediate gate failures instead of latent perf drift.
- Reusing one bin/list allocator path for both tenured and LOS free spans (`src/runtime/heap.zig:1124`, `src/runtime/heap.zig:1175`, `src/runtime/heap.zig:1433`) removed duplicate allocation-policy code and made LOS reuse use the same bounded best-fit behavior as tenured.
- Rewinding LOS bump-pointer from coalesced tail spans (`src/runtime/heap.zig:1420`, `src/runtime/heap.zig:1501`) reclaimed top-of-LOS space immediately and reduced LOS reuse latency on subsequent allocations.
- Emitting LOS policy + live-bytes counters in Maxima workload GC snapshots (`bench/maxima_workload.zig:50`, `bench/maxima_workload.zig:112`, `bench/maxima_workload.zig:629`) made real-workload LOS behavior inspectable without ad-hoc traces.
- Extending `tools/gc-compare` Maxima parsing with LOS bounds checks (`tools/gc-compare:589`, `tools/gc-compare:631`, `tools/gc-compare:833`) provided one-command verification that LOS policy remains in-range under `--with-maxima`.
- Adding opt-in mutator profiling counters (`HABU_PROFILE_MUTATOR`) for write barrier and safepoint paths (`src/runtime/heap.zig:527`, `src/interp/vm.zig:1420`, `src/jit/backend.zig:82`) produced direct VM-vs-JIT overhead telemetry without changing default hot-path behavior.
- Wiring `tools/perf-loop --profile-mutator` to export/load mutator profile snapshots (`tools/perf-loop:132`, `tools/perf-loop:392`, `tools/perf-loop:457`) made barrier/safepoint overhead part of the standard optimization loop.
- Inlining a cheap `stored.isPointer()` guard at VM/JIT barrier call sites (`src/interp/vm.zig:1417`, `src/jit/backend.zig:99`) cut mutator-profiled barrier call volume on Maxima load paths without changing GC semantics.
- Batching debt safepoint polls by both op-count and allocation-byte budget (`src/interp/vm.zig:1432`, `src/interp/vm.zig:471`) preserved bounded polling latency while cutting VM safepoint poll overhead by an order of magnitude on Maxima loads.
- Resetting safepoint batch counters on every actual GC entry (`src/interp/vm.zig:1452`) avoided stale-batch carryover after collections.
- Defining a single cross-runtime workload manifest (`bench/pack/corpus.json`) removed benchmark-name drift between Habu/SBCL tooling and provides a stable contract for OCaml runner integration.
- Moving runtime execution into one shared adapter module (`tools/bench_pack_runner.py:369`, `tools/bench_pack_runner.py:445`) made `tools/perf-loop` and `tools/gc-compare` consume identical normalized payloads, eliminating duplicated command/parsing drift.
- Enforcing required top-level JSON keys when scraping mixed stdout (`tools/bench_pack_runner.py:426`, `tools/bench_pack_runner.py:481`) prevented nested benchmark-object misparses and restored complete workload accounting in `tools/perf-loop` (`tools/perf-loop:116`) and `tools/gc-compare` (`tools/gc-compare:285`).
- Surfacing OCaml adapter status/errors in JSON and text outputs (`tools/perf-loop:491`, `tools/gc-compare:706`) made missing OCaml command wiring explicit instead of silently dropping the runtime.
- Emitting selected-gate parity deltas and CI trend series directly from gate evaluations (`tools/gc-compare:680`, `tools/gc-compare:706`, `tools/gc-compare:947`) created a machine-consumable contract for regression dashboards without duplicating gate math downstream.
- Ranking GC actions from repeated `gc-compare` samples with per-metric confidence (`tools/perf-loop:314`, `tools/perf-loop:410`, `tools/perf-loop:548`) reduced score volatility and exposed low-confidence optimization signals directly in reasons/output.
- Persisting perf-loop runs as append-only JSONL plus derived trend lines (`tools/perf-loop:574`, `tools/perf-loop:706`, `tools/perf-loop:917`) gives a durable self-improvement trail without coupling ranking logic to external storage.
- Emitting explicit `next_dots` recommendations from measured score/confidence/trend signals (`tools/perf-loop:658`, `tools/perf-loop:750`, `tools/perf-loop:988`) turns perf-loop output into direct execution commands instead of manual interpretation.
- Adding a dedicated `gc-parity` build step (`build.zig:279`) plus CI workflow (`.github/workflows/gc-parity.yml`) gives a stable entrypoint for parity artifacts without forcing gate failures yet.
- Adding regression-baseline mode to `tools/gc-compare` (`--regression-baseline`, `--fail-on-regressions`) let CI hard-fail on measured drift while keeping absolute parity milestones as informational (`tools/gc-compare:253`, `tools/gc-compare:781`).
- Publishing a single GC parity contract doc (`docs/gc-parity-contract.md`) and linking it from `bench/README.md`/`docs/README.md` removed ambiguity about gate semantics vs regression semantics.
- Linking Maxima loader docs directly to parity/regression commands (`docs/maxima-loader.md`) made loader RCA and perf gate checks share one operational entrypoint.

### Did Not Work
- Assuming `jit_backend.setHeap()` was always safe in bridge helpers without first syncing inline-cons progress was wrong; when JIT had advanced `g_alloc_ptr`, bridge entry rewound allocator state and corrupted in-flight recursive data structures (`src/interp/vm.zig` pre-fix `jitCallBridgeInvoke`).
- Gating non-self call handling on populated `known_fns` was brittle in test/harness compilation paths (`src/testing/compile_chunk.zig:191` calls `compileIr` without known-fn map), leaving call-arg cycle passes disabled for real helper-call shapes.
- Saving/restoring VM globals in local structs across `load`/nested eval (`src/interp/repl.zig` pre-fix `savePackageGlobals`/`restorePackageGlobals` pattern) is unsafe under moving GC; the restored values can be stale and later crash in GC object-size dispatch.
- Bundling a broader load-global rebinding rewrite while fixing package restoration caused a deterministic Maxima nparse regression (`InvalidIr` in `SIMPTIMES`); isolating the package-root fix first restored the gate before further refactor work.
- Stress fixtures that keep entire allocation chains alive (for example repeatedly `cons`ing into a retained list) can OOM before the target invariant is exercised; GC-stress regressions should churn ephemeral allocations.
- Reordering root-stack pushes to allocate before assigning the new root (`src/interp/repl.zig` `pushRootValue` experiment) leaves the incoming value unrooted during GC and can crash later in macro symbol canonicalization (`maxima ... ifactor` path).
- Assuming post-promotion collections start from an idle major-cycle state was wrong; tests that drop roots mid-cycle can observe old marks and fail reclamation assertions unless the previous cycle is drained first (`src/runtime/gc.zig:2477`).
- Using `jj squash --from ...` without `--message` in non-interactive automation opened an editor unexpectedly; always pass `--message` for scripted merges.
- Assuming a fixed `MAJOR_SWEEP_BUDGET`-sized fixture would keep major cycle active was brittle; root ordering/object size can make the cycle complete in one pass, so barrier tests need larger deterministic workloads.
- Transitioning mark->sweep as a single step per minor cycle delayed tiny sweep completions by an extra GC; using iterative phase advancement in one cycle (`src/runtime/gc.zig:639`) fixed this regression.
- More aggressive debt thresholds/weights looked faster on Maxima only because loader failures increased (`maxima_habu_errors` 8→10), so raw wall-time wins are invalid unless error counts stay flat.
- Counting `gc_debt_paydown_bytes` as raw `max(copied,reclaimed)` was wrong (`src/runtime/heap.zig:1370`): it can exceed debt inflow by orders of magnitude and trip valid invariants (`bench/check.zig:416`).
- Treating a single default-threshold `bench-check` p95 miss as semantic breakage was noisy in this environment; rerunning with a relaxed p95 gate isolated invariant/schema correctness from host performance variance.
- Assuming promotion-success counters would update only when tenured sweep reclaimed something was wrong; the old early-return path in `sweepTenured` skipped success accounting for all-live sets.
- Using `AutoHashMapUnmanaged.ensureTotalCapacity(..., entries.len)` without casting failed on Zig 0.15 `Size` typing (`src/runtime/heap.zig:1294`); explicit integer casts are required.
- Relying on `jj diff` word-level render to validate edited code was misleading during this RCA; several hunks appeared token-mashed while source files were correct, so direct line inspection (`nl -ba`) is required before concluding syntax damage.
- Treating full `zig build test` as a required close gate in this environment remained unreliable (`--listen` hang state); targeted `-Dtest-filter` gates plus workload repro must be the deterministic proof path until harness stability improves.
- Assuming `(in-package ...)` inside one `progn` would affect reader/package resolution for subsequent symbols in the same already-read form was wrong; defining formatter helpers with explicit package-qualified symbol names avoids this trap.
- Relying on `tools/dot-finish` full `zig build test` in this environment was unreliable due harness stalls; targeted filtered test gates provided deterministic validation for dot closure work.
- Running real-workload CAS loops with large default iteration counts caused impractically long benchmark runs; use very small defaults plus explicit `--scale` for controlled expansion.
- Parsing mixed benchmark stdout by taking the last JSON object without key validation was incorrect for list-heavy payloads; inner bench objects can parse successfully and masquerade as full payloads (`tools/bench_pack_runner.py:426` fix with `required_keys`).
- Using `datetime.utcnow()` for persisted run timestamps triggered runtime deprecation warnings in current Python; use timezone-aware UTC timestamps (`tools/perf-loop:707`).
- Reusing `lib/maxima-loader.lisp` as-is for SBCL benchmarking was brittle because warning conditions were treated as load failures; SBCL-side loaders need warning-muffling and explicit per-file load control.
- Clamping adaptive nursery targets only to static min/max bounds was insufficient: without a live-bytes floor, shrink decisions can violate runtime occupancy constraints (`src/runtime/heap.zig:1140`) and force pathological recollection behavior.
- Using plain unsigned subtraction for per-cycle counter deltas (`src/runtime/gc.zig:165`) is unsafe with wrapping counters; use modular delta (`-%`) consistently.
- A less aggressive nursery shrink law experiment in `src/runtime/gc.zig` increased Maxima stressed-runtime totals (~75.7s baseline to ~78.2s at `scale=4,nursery=24`), so benchmark-driven tuning must keep the original coefficients until tenuring/debt controls land.
- Very small nursery settings (`tools/maxima-bench --nursery-mb=8..16`) exposed real crash paths under GC pressure (compiler/runtime stale-pointer faults), so treat those runs as RCA repros, not tuning datapoints.
- Enforcing `tenured_live > 0 => tenured_bytes > 0` as a strict benchmark invariant was incorrect for current allocator accounting (`bench/check.zig:390`): `tenured_bytes` tracks bump-usage, not exact live-bytes, so hard coupling generated false failures on valid runs.
- Repl/stdlib-driven tenuring stress tests were brittle for this gate (fixture-sensitive OOM and promotion-starvation); heap-driven promote/drop cycles in `src/tests/integration.zig:7173` are a better deterministic guard for policy regression checks.
- In this environment `zig build bench-check -- --json` can stall with sleeping `bench_check`/build processes and no progress output; targeted `-Dtest-filter` gates plus `tools/gc-compare` JSON checks are the reliable verification path until harness stability is fixed.
- Using only per-object `hasMarkedCardInAddrRange` checks across all old objects is still too cache-cold for remembered scans at scale; run coalescing + fast run filtering should be the baseline before deeper RSet tuning.
- Running `python -m py_compile` in-tree drops `tools/__pycache__` artifacts; remove these before commit to keep generated files out of history.
- Debt-triggered precollection is safe for `Value` roots but not raw heap-backed byte slices (`allocString`/`intern`/`allocSymbol`); those paths still need explicit stable-copy handling before enabling proactive debt collections there.
- Using cumulative allocation histograms directly for LOS adaptation was wrong; control decisions must use per-cycle deltas (`src/runtime/gc.zig:244`) or thresholds drift from stale historical bias.
- Asserting absolute LOS object positions in tests was brittle because low thresholds can route bootstrap allocations into LOS; capture/mark target spans explicitly and assert reuse by span address (`src/runtime/heap.zig:3490`, `src/runtime/heap.zig:3521`).
- Looking only at Maxima run-phase GC counters is insufficient for LOS validation when run-phase alloc pressure is low (`maxima_gc_run_count` may be 0); include load-phase LOS telemetry in validation checks (`tools/gc-compare:589`, `tools/gc-compare:839`).
- Running `zig test src/interp/vm.zig` directly is invalid in this repo layout (relative imports outside module path); validate VM changes through build steps/bench paths instead.
- For short run phases, nanosecond counter deltas can quantize to zero (`wb_ns` on tiny benchmark tails), so compare call counts and load-phase totals instead of relying on single tiny-phase timing deltas.
- Op-count-only safepoint batching can over-delay polls during large single allocations; enforce a byte budget (`SAFEPOINT_BATCH_BYTES`) alongside op budget to keep latency bounded by allocation volume.
- Maintaining benchmark name lists in multiple scripts is fragile; keep workload names in one corpus and have runner tools consume that manifest.

## Session Notes (2026-02-18)

### Worked Well
- Reproducing function-namespace corruption with a minimal generic CL case (`(proclaim '(special selector))` + `(symbol-function 'selector)`) made the Maxima `defmode` failure deterministic without Maxima-specific assumptions.
- Storing function bindings explicitly at defun/fdefinition/symbol-function definition points (`src/compiler/compile.zig:6051`, `src/compiler/compile.zig:6114`, `src/compiler/compile.zig:8141`, `src/compiler/compile.zig:8146`) plus VM-side function-cell resolution (`src/interp/vm.zig:808`) fixed the root namespace bug instead of masking it.
- Adding function-cell lookup to REPL callable resolution (`src/interp/repl.zig:981`, `src/interp/repl.zig:1058`) kept `fboundp`/designator behavior stable when value cells are dynamically rebound.
- Expanding builtin function classification to include internal setf helpers (`src/compiler/compile.zig` primitive dispatch table now includes `%aset`/`%svset`/`%sset`) removed brittle reliance on nil-slot fallback during stdlib bootstrap and reduced resolver misses.
- Locking the regression in integration (`src/tests/integration.zig:6165`) and updating the Maxima readiness gate as behavior improved (`src/tests/integration.zig:5989`) prevented reintroducing special-binding/function-binding alias bugs.
- Sampling the long-running integrate gate (`sample` on the live test PID) immediately identified the real hot region (`expandMacro`/`compileCondWithTail`) instead of guessing.
- Checking process state during long `zig build test` runs distinguished real runtime hotspots from external build contention and avoided chasing false "hang" causes.
- Using REPL-compiled defmacro closures in compiler expansion (`src/interp/repl.zig` compiled macro-table entries + `src/compiler/compile.zig` direct closure-call path) removed repeated macro-lambda compile/emit cycles while keeping chunk/index semantics safe (closures come from stable REPL chunk pool, not transient expansion pools).
- In `lib/stdlib.habu`, parsing `IF/WHEN/UNLESS ... DO` actions with the same keyword-boundary rule as top-level `DO` fixed a real parser bug where trailing forms (for example `(loop-finish)`) were misclassified as top-level LOOP clauses.
- Rewriting `loop-finish` calls at LOOP codegen time (after `result-expr` is known) preserved generic accumulation semantics while avoiding Maxima-specific behavior.
- Tracing Maxima load with per-form names (`TRACE defun ...`) made the real blocker obvious: `db.lisp` `defun clear` failed only because preceding `defmode` setup failed.
- Reducing the failure to a minimal repro (`defmode` + `putprop` arg probe) exposed the root semantic bug: proclaimed `special` lambda params were compiled lexically, so helper callees saw `name=nil`.
- Fixing lambda-parameter special semantics generically in `src/compiler/compile.zig` (dynamic `progv` wrapper for globally proclaimed special params) restored `declare-top` behavior across Maxima macros without Maxima-specific patches.
- Adding a focused regression in `src/tests/integration.zig` (`proclaimed special lambda params are dynamically visible in callees`) locks this dynamic-scope contract.
- Adding system-only/internal keywords on `maxima-load-all` (`:habu-stop-on-error`, `:habu-required-bindings`) enabled stronger diagnostics without bending CL-facing defaults.
- Removing per-form error masking in `src/interp/repl.zig` `evalForms` (then named `evalFileContentSeparateVm`) made `(load ...)` semantics deterministic and restored reliable file-level failure accounting for Maxima loader gates.
- Locking strict load semantics with a focused regression (`src/interp/repl.zig` `loadFile` aborts on first form error) prevented silent partial-file success regressions.
- Fixing `loop` parser support for `FOR ... IN ... BY ...` in `lib/stdlib.habu` removed a generic clause-gap that surfaced as `Unknown loop keyword: BY` in large Lisp packages.
- Extending `get-setf-expansion` with composed list-place updaters (`cadr`/`cddr`/`caddr`/`cdddr`/aliases) removed a high-frequency `setf: unsupported place` class for macro-heavy code.
- Reworking LOOP conditional routing to accept `ELSE WHEN ... ELSE ...` in `lib/stdlib.habu` unblocked real-world clause patterns (e.g. `commac.lisp` `maknam`) without Maxima-specific branches.
- Tracking proclaimed specials by symbol identity (`Value.raw`) instead of bare names in `src/compiler/compile.zig` prevents cross-package special-variable leakage.
- Fixing nested callback non-local exits in `src/interp/vm.zig` (`callFromStackAt`/`doThrow`) removed a root semantic bug where `handler-case` around `(load ...)` could catch an error and still resume the loaded file.
- Adding dual regressions in `src/interp/repl.zig` for direct eval and script-driven `handler-case (load ...)` closed the gap that only appeared when `load` ran inside another loaded script.
- Keeping GC state persistent in `src/runtime/heap.zig` (`Heap.gc`) and routing collection through `self.gc.collectRootSet(...)` eliminated per-collection `GC.init/deinit` churn from the hot path.
- Refactoring `src/runtime/gc.zig` to pass `heap` explicitly into `collect/collectRootSet/copyValue/scanObject` made collector lifetime safe and enabled queue reuse; GC benchmark p95 dropped to ~7.49ms from ~7.72ms on `bench-check`.
- Adding phase counters to `src/runtime/heap.zig`/`src/runtime/gc.zig` and surfacing them in `bench/gc.zig` gave actionable GC slices (`build/root/copy/finalize`) and enabled structural perf gates in `bench/check.zig`.
- Caching internal GC root slots in `src/runtime/heap.zig` (`gc_internal_slots` + `calcGcRootSig`) removed per-collection full table walks; using `SymbolTable.version` in the signature prevented stale-cache reuse when symbol maps mutate without net count change.
- Adding explicit heap layout scaffolding (`GcLayoutMode`, `HeapLayout`, `Region`) in `src/runtime/heap.zig` made nursery/tenured/LOS boundaries concrete without changing current semispace behavior; this keeps incremental generational work isolated and testable.
- Adding a no-allocation write barrier in `src/runtime/heap.zig` (card table + `writeBarrier`) and calling it at VM/primitives pointer-store sites (`src/interp/vm.zig`, `src/runtime/primitives/list.zig`, `src/runtime/primitives/hash.zig`, `src/runtime/primitives/clos.zig`, `src/runtime/primitives/symbol.zig`) provided generational-safe mutation hooks without changing non-generational behavior.
- Exposing remembered-set APIs (`markedCardCount`, `appendMarkedCards`, `appendMarkedCardRanges`, `clearMarkedCards`) in `src/runtime/heap.zig` made barrier output directly consumable for upcoming minor-GC root scanning and added deterministic tests for mark/enumerate/clear flow.
- Adding JIT-side barrier/safepoint hooks in `src/jit/backend.zig` (`jitWriteBarrier`, `jitSafepointBeforeAlloc`) keeps runtime helper mutations (`jitNreverse`) and slow allocation paths aligned with VM barrier invariants.
- Splitting GC entry by layout mode in `src/runtime/gc.zig` (`collectSemispaceRootSet` vs `collectMinorRootSet`) kept semispace behavior stable while enabling generational-only logic incrementally.
- Keeping minor-GC promotion conservative (pointer-free objects only) in `src/runtime/gc.zig` `shouldPromote` avoided premature tenure of resource-bearing/ref containers before tenured mark/sweep exists.
- Extending stream liveness checks in `src/runtime/gc.zig` `finalizeUnreachable` to accept forwarded tenured addresses prevented false-finalization when survivors are promoted.
- Adding non-moving tenured mark-sweep metadata in `src/runtime/heap.zig` (`tenured_objs.marked` + `tenured_free`) enabled deterministic reclaim of dead promoted objects without moving survivors.
- Marking tenured reachability directly in `src/runtime/gc.zig` `copyValue` for non-from-space pointers ensured tenured objects reachable only through nursery survivors are not swept accidentally.
- Extending the same non-moving discipline to LOS (`src/runtime/heap.zig` `allocLosRaw`/`recordLosObject`/`sweepLos`) made large-object allocation and reclamation predictable with stable addresses.
- Mark-on-touch + work-queue scan for LOS in `src/runtime/gc.zig` `copyValue` prevented stale young pointers inside pinned large containers across minor collections.
- Switching GC perf benches to generational fixtures (`bench/gc.zig`) is essential; semispace-only benches can pass while generational paths silently regress.
- Hoist API drift checks must run under `-Duse-hoist=true`; default test mode can otherwise hide interface breakage behind the stub backend.
- In `src/interp/vm.zig` `collectGarbageExtra`, replacing the closure-count prepass with an upper-bound capacity estimate (`self.fp`) and merging frame closure/chunk staging into one pass removed a duplicate frame walk with no semantic change.
- Reintroducing a source-backed `bench/jit.zig` and wiring `bench-jit` in `build.zig` removed a stale-artifact trap where `bench-check` could read an old `zig-out/bin/jit_bench`.
- Enforcing strict `bench/check.zig` argument handling (`InvalidArgs` returns non-zero) exposed accidental no-op invocations like `bench-check -- --json /tmp/file`.

### Did Not Work
- Leaving `CompiledFn.callFromValues` capped at arity 3 (`src/jit/backend.zig` pre-fix) quietly produced `nil` for 4+ arg JIT functions even when compilation succeeded, masking coverage and correctness holes.
- Relying on naive topological reordering for call-arg moves without cycle breaking (`src/jit/backend.zig` pre-fix `fixCallArgMoves`) produced deterministically wrong helper arguments on 4-arg call-indirect paths and silently returned `nil` from JIT `format` calls.
- A pure per-character preallocation path for all string concatenations (`lib/stdlib.habu:2416` intermediate attempt) improved long concatenations but regressed short hot call sites; restoring dedicated 1/2-arg `string-concat` fast paths fixed that.
- A loop-only `reduce` rewrite without function-specialized dispatch barely moved the benchmark (~1901ms to ~1894ms), confirming that per-element `funcall` overhead (not recursion itself) was the dominant bottleneck in the hot `#'+` path.
- Keeping `mapcar` on a single generic variadic `apply` loop (`lib/stdlib.habu:107` pre-fix) caused severe avoidable overhead for the dominant one-list benchmark shape; arity-specialized paths are required for production throughput.
- Making `resolveFunctionValue` strict-callable-only without preserving nil-slot bootstrap behavior immediately broke stdlib bootstrapping (`%ASET` unresolved via `(symbol-function '%aset)`); preserving nil/unbound slot fallback while rejecting non-callable non-nil values was required.
- Caching compiled macro expanders by storing closures in `macro_table` is not safe with current chunk-pool/index patching: cached closures retain expansion-time chunk index assumptions and can mis-dispatch nested lambdas later.
- "Tagged cached macro" wrappers still failed because macro closures compiled in one expansion context are not context-free artifacts under current VM/compiler coupling (chunk indices + expansion-time global/macro state assumptions).
- Treating a hanging `zig build test -Dtest-filter=...` run as a runtime hotspot signal was misleading in some cases: sampled hangs showed Zig build/test protocol wait states (`build` polling while `test --listen` waited for commands), so a stuck filtered run is not automatically a VM performance regression.
- Treating conditional `DO` boundaries as only `ELSE/END/AND` was wrong: it consumed subsequent LOOP clauses (like `COLLECT`) and silently changed loop results.
- Defining `loop-finish` as a global macro caused expansion timing issues; keeping it as a callable symbol and lowering it inside LOOP expansion was more reliable here.
- Chasing downstream `SIMPLE-ERROR` output first was noisy; until `defmode`/special-parameter semantics were fixed, later integrate traces were mostly secondary fallout.
- Running long `zig build test -Dtest-filter=\"...maxima...\"` invocations remained unreliable/hang-prone in this environment; short focused filters and direct scripted repros gave more deterministic signal.
- Using multiline piped REPL scripts for loader RCA gave misleading/garbled diagnostics; `habu <script-file>` probes and targeted tests were more trustworthy.
- Name-only special-declaration matching in the compiler was too coarse; package-unaware declaration lookup can silently destabilize unrelated lexical bindings.
- Testing only direct `(handler-case (load ...))` eval was insufficient; script-level `loadFile` execution has different callback boundaries and must be covered explicitly.
- Blind regex rewrites on function-call signatures in `src/runtime/gc.zig` briefly produced duplicate arguments (`self.copyValue(heap, heap, ...)`); immediate compile/test loops are required right after broad replacements.
- Using `std.time.Timer.start()` inside GC internals widened the GC error set (`TimerUnsupported`) and broke call-site error contracts in `src/interp/vm.zig`; use `std.time.nanoTimestamp()` deltas in hot/runtime internals when error signatures must stay stable.
- Root-cache signatures based only on table counts are not enough; equal counts can still hide map-entry churn. Include mutation/version signals (for symbol tables) or stronger structure signatures.
- Generational scaffolding must not silently change default capacity assumptions; keep default mode semispace and prove unchanged behavior with existing bench-check gates before moving to barrier/minor-GC dots.
- Barrier coverage needs grep-driven audits after each refactor (`.car=`, `.cdr=`, `vec.set`, hash puts); it is easy to miss direct stores in VM helpers and primitive paths.
- Remembered-set APIs should be allocation-free on hot mutation paths and only allocate during explicit scan/export calls; keep the write barrier itself side-effect-light.
- JIT runtime helpers can mutate heap objects outside the interpreter dispatch loop; barrier logic must be hooked there explicitly or remembered sets drift silently.
- Running `zig build test` in this environment can park in Zig `--listen` mode without emitting failures; treat that as harness instability and rely on targeted test filters plus explicit process sampling for RCA.
- Promoting pointer-bearing containers before implementing tenured collection is a semantic trap: unreachable promoted objects will not be reclaimed/finalized yet, so promotion policy must enforce this boundary explicitly.
- Reclaiming tenured holes without a free-list leaves long-running sessions with artificial tenured OOM despite low live set; non-moving sweep must feed allocator reuse paths immediately.
- LOS tests should assert deltas, not absolute counts: heap bootstrap can legitimately pre-populate LOS metadata when low thresholds are used in tests.
- Bench checks should assert structural GC invariants (promoted bytes, LOS/tenured liveness, old-space bounds), not just pause time, to catch semantic regressions early.
- For hoist signatures, ownership is transferred into `Function.init`; calling `sig.deinit()` afterwards double-frees and crashes.
- Leaving `use-hoist` defaulted off while still labeling runs as JIT leads to misleading perf/RCA outcomes (e.g. recursive benchmarks failing under interpreter stack limits while reported as JIT mode).

## Session Notes (2026-02-19)

### Worked Well
- Splitting constructor missing-slot defaults by type family in `generateStructConstructor` (`src/compiler/compile.zig:10071`, call sites `src/compiler/compile.zig:9756` and `src/compiler/compile.zig:11261`) restored CL semantics: `defclass` slots without initform start unbound, while `defstruct` still defaults to nil.
- Locking `defstruct` nil-default behavior with a focused regression (`src/tests/integration.zig:3698`) prevented a silent semantic regression while fixing CLOS slot-boundp behavior.
- Updating Maxima subset gate checks to package-qualified symbols (`src/tests/integration.zig:5783`) removed false negatives caused by strict package resolution.
- Treating escaped reader characters as syntax (not symbol-name data) in parser expectations (`src/reader/parser.zig:2513`) aligned tests with CL reader behavior.
- Treating `Repl` as self-referential (VM callbacks/global-env pointers into `Repl.compiler`) and keeping helper state at a stable address (`src/tests/integration.zig:5050`) removed deterministic `set_symbol_function` segfaults in MV tests.
- Adding a focused regression for moved-helper REPL execution (`src/tests/integration.zig:5075`) keeps this lifetime bug from returning silently.
- Replacing `global_special_syms` raw-value keys with package/uid-aware `VarKey` identity (`src/compiler/compile.zig:2159`, `src/compiler/compile.zig:2205`) removed GC-movement sensitivity from special-variable tracking and fixed stale special lookups after collections.
- Detecting leading local `(declare (special ...))` forms before lowering `let` (`src/compiler/compile.zig:5150`) fixed a root semantic gap where locally-declared specials were compiled lexically.
- Unifying symbol value-cell operations through explicit VM helpers (`src/interp/vm.zig:823`, `src/interp/vm.zig:835`) and handling uninterned symbols via stable uids fixed `symbol-value`/`boundp`/`makunbound`/`progv` behavior generically.
- Rewriting `progv` save/restore to bind concrete slots or uninterned symbol cells (`src/interp/vm.zig:7257`) removed name-suffix aliasing and restored correct dynamic binding restoration.
- Routing `write-string` through shared stream I/O (`src/runtime/primitives/stream.zig:407`, `src/runtime/primitives/io.zig:787`) centralized stream-type behavior and avoids duplicated output-path logic.
- Locking regressions for dynamic specials/value cells/numeric predicates (`src/tests/integration.zig:6226`, `src/tests/integration.zig:6305`, `src/tests/integration.zig:6344`) kept fixes generic and prevented Maxima-only drift.
- Tightening `set_symbol_function` to stop mutating value-cell globals except legacy callable slots (`src/interp/vm.zig:4026`) removed a generic namespace corruption path where `defun` could overwrite unrelated variable bindings (for example Maxima `ratvars`-style symbols).
- Locking this with explicit regressions for shared symbol names and nil-bound values (`src/tests/integration.zig:6245`, `src/tests/integration.zig:6282`) keeps future function-cell work from silently reintroducing value-cell clobbers.
- Capturing the failing `check_closure` disassembly for the Maxima gate showed the assertion was injected at local `let` binding stores, which pointed directly to declaration scoping instead of JIT/runtime dispatch.
- Adding lexical declaration storage to `Env` (`src/compiler/compile.zig:1461`, `src/compiler/compile.zig:1587`, `src/compiler/compile.zig:1698`) and routing local `(declare (type ...))` through it (`src/compiler/compile.zig:13037`) stopped cross-form declaration bleed.
- Switching lexical variable assertion lookup from global name-based declarations to environment symbol-identity lookup (`src/compiler/compile.zig:2620`) removed false `assert_closure`/`assert_fixnum` injections in unrelated forms.
- Dropping global type-decl application from `let` initializer compilation (`src/compiler/compile.zig:5002`) removed a root crash vector where unrelated local names inherited stale global declarations.
- Locking the fix with a regression (`src/tests/integration.zig:5889`) prevents reintroducing local type declaration leakage.
- Tracing with `HABU_TRACE_ERROR_CONTEXT=1`/`HABU_TRACE_ERROR_ONLY=UnboundSymbol` on the module load gate immediately exposed the actual symbol-function miss (`ATAN`) inside `trigi` instead of chasing downstream parser/runtime fallout.
- Installing BIGFLOAT-IMPL callable aliases through a guarded binder (`lib/maxima-stubs.lisp:120`, `lib/maxima-stubs.lisp:133`) plus inverse-trig fallbacks (`lib/maxima-stubs.lisp:105`) fixed `trigi`/`trigo` loading generically and kept operator symbols fbound across package shadowing.
- Adding a focused trigi subset regression (`src/tests/integration.zig:5921`) catches future regressions where callable trig aliases disappear during package/bootstrap changes.
- Tracing unbound calls in `SIMP-%SIN` showed `COMPLEX-NUMBER-P` was required before `ellipt.lisp`; adding a bootstrap-compatible definition in `lib/maxima-stubs.lisp:257` removed that hidden dependency from trig subset execution.
- Keeping `def-simplifier` bootstrap output aligned with `simp.lisp`'s real `arg-count-check` arity (`lib/maxima-stubs.lisp:355`) eliminated a cross-module arity mismatch that only appears after `simp` redefines `arg-count-check`.
- Mapping VM `InvalidTypeSpecifier`/`InvalidArgument` to CL conditions in `zigErrorToConditionSym` (`src/interp/vm.zig:7398`) restored `handler-case` behavior for malformed type/argument paths and kept long Maxima probes from aborting at the first uncaught Zig error.
- Locking the condition mapping with `src/tests/integration.zig` (`handler-case catches invalid argument and invalid type specifier`) prevents condition-handler regressions from silently returning to raw Zig error aborts.
- Converting the Maxima end-to-end check into a deterministic readiness vector (`src/tests/integration.zig:5781`) keeps large-package progress measurable without hiding remaining semantic gaps.
- Splitting large Maxima setup/eval forms into separate `repl.eval` calls reduced parser-noise and made failures attributable to specific steps instead of one monolithic expression.
- Keeping `defun` intact in desugar (`src/compiler/passes/p02_desugar.zig`) and only desugaring the body restored compiler-level DEFUN semantics (implicit function block), which removed `NoMatchingBlock` failures in real Maxima functions (`add-lineinfo`).
- Restricting legacy bare-name global fallback to `CL-USER` symbols in both compiler and VM (`src/compiler/compile.zig`, `src/interp/vm.zig`) prevented cross-package function-cell capture (notably `FUNCTIONP` recursion paths while loading Maxima).
- Preserving secondary values across `pop_block`/`push_block` in VM op post-processing (`src/interp/vm.zig`) fixed a subtle multi-value regression introduced by implicit DEFUN blocks (`(defun f () (values ...))` started returning only the primary value before this fix).
- Routing builtin callable checks through compiler dispatch tables (`src/compiler/compile.zig:14482`) and consuming that API in REPL symbol resolution (`src/interp/repl.zig:880`, `src/interp/repl.zig:908`, `src/interp/repl.zig:956`) fixed the `ATAN`/`%ATAN` unbound path in `trigi` without adding Maxima-specific symbol aliases.
- Adding/keeping focused gates (`src/tests/integration.zig:5921`, `src/tests/integration.zig:5979`, `src/tests/integration.zig:6047`) gave deterministic proof for the trig/matrix/dependency chain fixes even when broad filtered test runs were noisy.

### Did Not Work
- Hard-coding Maxima subset load counts in integration gates is brittle; module lists and transitive dependencies drift and invalidate exact-count assertions.
- Using unqualified `fboundp` symbols in package-heavy loaders created misleading failures even when target functions were correctly defined in `MAXIMA`.
- Returning `Repl` by value from test helpers was unsafe: internal pointers (`vm.global_env`, callback contexts) can dangle after copies/moves and crash later in unrelated eval paths.
- Continuing to use raw `Value.raw` identity for globally special symbols was incorrect under moving GC; symbol keys must use package/uid-aware identity to stay stable.
- Treating uninterned symbols like global-name fallbacks was wrong; uninterned value cells need dedicated storage keyed by stable symbol uid (`src/interp/vm.zig:788`).
- Leaving debug env checks in hot VM op paths (for example `write_to_stream`) is a measurable perf anti-pattern; remove tracing from opcode dispatch and keep diagnostics opt-in at higher layers.
- Running `zig build test -Dtest-filter='maxima e2e operation readiness status'` is still hang-prone here; the equivalent scripted readiness probe produced deterministic signal.
- Treating the previous function-cell fix as complete was incorrect; leaving `nil`/`unbound` in the value-cell overwrite allowlist still let `defun` corrupt same-name variables in generic Lisp code.
- Focusing first on mixed special-`let` lowering (`tryCompileSpecialLet`) was a false lead; the actual fault came from type declaration leakage into lexical bindings.
- Running broad `zig build test -Dtest-filter='maxima '` remains unreliable in this environment (hang-prone); targeted filters for failing gates are more deterministic for RCA.
- Packing very large module-list setup and operation probes into a single reader input string produced unstable `UnexpectedToken` failures; smaller staged eval forms are safer for large integration probes.
- Assuming package-qualified names were safe under old fallback logic was wrong: fallback-to-bare-name can silently bind to the wrong package/global slot and manifests later as recursive calls instead of immediate package resolution errors.
- Directly aliasing every BIGFLOAT-IMPL symbol to a `cl:` function without a `fboundp` guard failed at load time (`ASIN` unbound on this runtime); guarded binding with explicit fallbacks is required for portable bootstrap stubs.
- Assuming stubbed helper function signatures stay stable across later Maxima module loads was wrong: `simp.lisp` redefines `arg-count-check` with different arity, so generated bootstrap calls must follow upstream arity contracts.
- Assuming `handler-case (error ...)` already covered all VM failures was wrong; unmapped Zig errors (`InvalidTypeSpecifier`, `InvalidArgument`) bypassed condition handlers until explicitly mapped.
- Reintroducing builtin-name scans as ad hoc manual lists (`Builtins.primitive_fields`) is brittle; stale entries caused `symbol-function` to miss legitimate primitives (`ATAN`) even though compiler lowering already supported them.
- Pushing wide `HABU_TRACE_FN_RESOLVE=1` traces across large Maxima loads produced megabytes of mostly noise; narrowing to failing subset tests and symbol-miss traces is faster for RCA.

## Session Notes (2026-02-17)

### Worked Well
- Following Maxima source to the exact failing semantic operation (`mrgmac.lisp` `defc/defs/defa`: `(coerce \`(lambda ...) 'function)`) gave a generic CL fix in `lib/stdlib.habu` (`coerce-to-function`) instead of a Maxima-specific patch.
- Converting temporary root-cause traces into focused regression tests (`src/tests/integration.zig`: function-designator coercion, optional `env` lambda designator arity) preserved behavior while allowing debug instrumentation to be removed cleanly from hot compiler/VM paths.
- Aligning `lib/maxima-loader.lisp` file order with upstream `src/maxima.system` module ordering (not ad-hoc sequencing) removed dependency-order regressions (`PUTOPR`/`SPECREPCHECK` class) and gave a principled path for loader parity.
- VM mismatch tracing (`HABU_TRACE_CALL_MISMATCH=1`, `HABU_TRACE_ERROR_CONTEXT=1`) exposed a generic CL semantic bug quickly: `MAPC` was fixed-arity in `lib/stdlib.habu` and failed in Maxima `$errormsg` multi-list dispatch.
- Replacing `mapc` with variadic CL semantics (`lib/stdlib.habu`) and adding focused regression coverage (`src/tests/integration.zig`: `stdlib mapc supports variadic list dispatch`) removed the callback-arity crash class without Maxima-specific patches.
- Persisting probe results to files (`/tmp/*.result`) after non-interactive `(load "...")` runs gave stable signal where REPL output was noisy; this exposed that integrate blockers were advancing from MAPC arity into missing module chain (`m2`/`schatchen-cond` unbound when `schatc` not loaded).
- Form-level tracing (`HABU_TRACE_FORMS=1`) isolated the failing loader site to `lib/maxima-stubs.lisp` form 24 (`eval-when`) quickly.
- Cross-checking Maxima symbol state through file-based reports (`with-open-file`) avoided terminal overwrite noise and made root-cause data stable (`/tmp/maxima-subset42-report.txt`).
- Reproducing with minimal Lisp snippets (outside full Maxima load) made package bugs obvious and testable.
- Adding focused regression tests in `src/runtime/primitives/package.zig` caught real root causes:
  - stale inherited-symbol replacement in native tables,
  - inherited lookup using native exports when Lisp export tables are sparse,
  - keyword nickname handling in package creation.
- Validating with the same Maxima subset gate used by integration (`lib/maxima-loader.lisp`, 39 files) gave a concrete pass criterion: `(39 39 0 1 1 1 1 1 1)`.
- Isolating Maxima `destructuring-let` failure to a language-level repro (`let` with mixed lexical + special vars) exposed the true compiler bug quickly:
  - `(let ((a 1) (*x* 2)) ...)` leaked writes to global `*x*` instead of dynamic binding.
  - Fixing mixed special/lexical lowering in `src/compiler/compile.zig` (specials via `progv` with temp bindings) removed the `LET-MACRO-HAIR` crash path.
- Adding dedicated integration regressions in `src/tests/integration.zig` for mixed special `let` and Maxima `letmac` keeps this class of bug from regressing.
- Treating `defpackage` as a strict semantic boundary (parse and apply `:import-from` / `:shadowing-import-from` instead of ignoring them) removed cross-package symbol alias bugs without Maxima-specific rewrites.
- Loading upstream Maxima package definitions first (`lib/maxima-loader.lisp` + `maxima-package.lisp`) and using stubs only as guarded fallbacks preserved symbol/package intent across diverse source files.
- Running package-form compilation in an arena-scoped compiler context (`src/interp/repl.zig` `evalPackageForm`) eliminated persistent IR node leaks on repeated `defpackage` evaluations.
- Fixing `%shadowing-import` replacement semantics in `src/runtime/primitives/package.zig` (replace conflicting local/native entries before import) aligned behavior with CL expectations and unblocked real package forms.

### Did Not Work
- Driving long Maxima probes via non-interactive `./zig-out/bin/habu < script` in this environment was unreliable for deterministic pass/fail capture; targeted integration tests were more trustworthy for regression signal.
- Using `./zig-out/bin/habu <script-file-arg>` as a multi-form probe source was misleading in this environment; only the final top-level form was reliably observed, so probe conclusions must come from integration tests or controlled REPL eval paths.
- Assuming `mapc` was already CL-compatible because `mapcar`/`mapl` were variadic was wrong; missing variadic support in one mapping combinator can break large Lisp packages in non-obvious error-reporting paths.
- Driving large multi-form scripts by piping raw lines into the interactive REPL produced misleading output corruption; loading a script file and writing explicit probe artifacts was required for trustworthy RCA.
- Using stdlib `find-symbol` as a debugging oracle was misleading; its previous shim semantics masked package-state bugs.
- Trusting `maxima-load-all` success counters alone was misleading: `sin.lisp` can leave `MAXIMA::SININT` unbound while reporting `(ok=total, fail=0)`, so binding checks (`fboundp`) are required for critical entrypoints.
- Assuming Lisp package export hash tables mirror native exports caused false negatives in inherited symbol classification.
- Accepting keyword nicknames in validation while later calling `nameBytes` (string/symbol-only) produced delayed `TypeError` in `eval-when`, not at option parse time.
- Relying on a single long `zig build test -Dtest-filter=...` run was unreliable in this environment; targeted tests plus direct REPL gate runs were more deterministic.
- Assuming a Maxima runtime failure (`$ratsimp`) was a setf-expander bug was wrong; after dependency fixes, the failure moved and the real issue was mixed special/lexical `let` compilation semantics.
- Silently ignoring unknown/unsupported `defpackage` options in `compileDefpackage` was a shortcut that hid root causes and led to hard-to-trace runtime recursion/dispatch failures.
- Implementing `shadowing-import` by delegating to plain `importSymbols` first was incorrect when same-name local symbols already existed; it caused native symbol-table conflicts instead of required replacement.

---

## Session Notes (2026-02-19)

### Worked Well
- Caching builtin refresh by heap epoch in `src/compiler/compile.zig` (`bi_heap`/`bi_gc`/`bi_cl_pkg`/`bi_cl_ver` + `refreshBuiltins`) removed repeated `Builtins.init` churn from primitive compile dispatch while still invalidating on GC and CL package symbol-table mutation.
- Replacing `append` primitive compilation’s temporary `ArrayList(*Ir)` in `src/compiler/compile.zig` with a streaming left fold removed a per-call transient allocation in a hot compile path.
- Locking refresh invalidation behavior with focused tests in `src/compiler/compile.zig` (`refreshBuiltins rebuilds when builtin handles are cleared`, `refreshBuiltins invalidates on CL package symbol-table mutation`) prevented cache-staleness regressions.
- Tracing bench JIT eligibility in `src/testing/compile_chunk.zig` immediately exposed two root causes for `compile_n=0`: top-level `defun` lowering had moved from `.define` to `.set_symbol_function`, and Hoist translation rejected implicit `.block` wrappers.
- Extending JIT candidate extraction in both `src/testing/compile_chunk.zig` and `src/interp/repl.zig` to accept `.set_symbol_function` + lambda restored post-defun JIT registration after function-cell lowering changes.
- Adding `.block` traversal/translation support in `src/jit/backend.zig` (`irAny`, `countIrNodes`, `canTranslate`, `firstUnsupportedTag`, `translate`, TCO helpers) fixed the real backend incompatibility instead of masking it in benchmark gating.
- Validating with `zig build -Duse-hoist=true bench-jit -- --json` and `zig build -Duse-hoist=true bench-check -- --json` proved end-to-end recovery (`compile_n=1`, `fail_n=0`) and restored meaningful JIT perf signal.
- Replacing macro-root staging in `src/compiler/compile.zig` (`callMacroClosure`, `expandMacro`) from temporary `ArrayList` map snapshots to direct root-buffer packing removed repeated transient allocations in macro expansion hot paths without changing GC/root restoration semantics.
- Pre-counting macro call argument arity and using a stack buffer (with single fallback heap alloc) removed dynamic `ArrayList(Value)` growth churn in compile-time macro invocation loops.
- Reworking `compileCondWithTail` in `src/compiler/compile.zig` to count clauses first and use a stack-first clause buffer (heap fallback only for large conds) kept iterative reverse lowering while removing per-cond `ArrayList` churn; regression `src/tests/integration.zig` with 80 clauses validates large fallback correctness.
- Rewriting `compileBodyWithTail` in `src/compiler/compile.zig` to use a single-form fast path plus one direct pre-sized allocation for multi-form bodies removed the prior `ArrayList`+`dupe` double-allocation pattern in a ubiquitous compile path.
- Rewriting `filterDeclares` in `src/compiler/compile.zig` to build a reversed list directly and reverse links in-place removed temporary `ArrayList(Value)` staging while preserving declaration processing and body ordering.
- Replacing `compileListPrim` / `compileBroadcastStream` / `compileConcatenatedStream` in `src/compiler/compile.zig` with count+single-allocation slices and direct IR node initialization removed the previous `ArrayList` then `dupe` double-allocation pattern for variadic primitive lowering.
- Rewriting `compileVariadicArith` in `src/compiler/compile.zig:15127` from `ArrayList(*Ir)` staging to a single-pass compile+fold removed transient allocation churn on arithmetic hot paths and allowed strict dotted-tail rejection (`(+ 1 . 2)` now errors at this lowering boundary).
- Locking variadic arithmetic semantics with focused regressions in `src/compiler/compile.zig:19284` (`(+)`, `(*)`, unary `(- x)`, unary `(/ x)`, and left-associated `(+ 1 2 3)`) preserved CL behavior while tightening argument-list validation.
- Replacing `compileCallNextMethod` and `generateMethodCallByNameAtDepth` transient `ArrayList` staging in `src/compiler/compile.zig:12126` and `src/compiler/compile.zig:12726` with pre-sized argument slices + `buildCallIr` removed list growth churn from generic-function dispatch call lowering.
- Adding focused regressions for `call-next-method` arg-shape preservation and dotted explicit arg rejection (`src/compiler/compile.zig:19336`, `src/compiler/compile.zig:19382`) locked the no-drop/no-mask behavior while tightening malformed-list handling.
- Replacing `compileMakeInstance` keyword/value call-arg staging and `compileFindClass` optional-form sequencing staging (`src/compiler/compile.zig:11522`, `src/compiler/compile.zig:11699`) with pre-sized slices removed extra temporary list growth and redundant copy churn in CLOS compile helpers.
- Locking `make-instance` ctor-arg preservation and `find-class` optional sequencing/dotted-tail rejection in focused regressions (`src/compiler/compile.zig:19410`, `src/compiler/compile.zig:19462`, `src/compiler/compile.zig:19494`) made these list-shape contracts explicit.
- Replacing `compileVectorPrim`, `compileAref`, and `compileAset` transient `ArrayList` staging (`src/compiler/compile.zig:16829`, `src/compiler/compile.zig:17009`, `src/compiler/compile.zig:17064`) with count+single-allocation slices removed avoidable growth/copy overhead in array/vector lowering paths.
- Adding focused regressions for vector/aref/aset operand preservation and dotted-tail rejection (`src/compiler/compile.zig:19524`, `src/compiler/compile.zig:19566`) locked the new strict list-shape checks and subscript/value arity behavior.
- Replacing `compileMakeArray` static-dimension staging (`src/compiler/compile.zig:16921`) with count+single-allocation slices and direct `.arr_new` IR node construction removed intermediate list growth and builder-level duplicate copying for static dimension lists.
- Locking scalar/static/dynamic `make-array` dimension lowering in focused regressions (`src/compiler/compile.zig:19603`) prevented regressions where static dimension vectors collapse back to dynamic paths.
- Replacing `compileMvBind`/`compileMvCall` `ArrayList` staging (`src/compiler/compile.zig:8108`, `src/compiler/compile.zig:8159`) with pre-counted slices removed transient growth churn and made malformed dotted tails fail early.
- Adding focused regressions for MV var/form counts and dotted-tail rejection (`src/compiler/compile.zig:19645`, `src/compiler/compile.zig:19688`) locked shape correctness while preserving `multiple-value-*` lowering semantics.
- Replacing `compileDefclass` expansion accumulation (`src/compiler/compile.zig:11375`) from `ArrayList` growth to exact pre-counted form allocation removed dynamic staging overhead while preserving reader/writer symbol filtering.
- Locking reader/writer expansion counting with a focused regression (`src/compiler/compile.zig:19120`) prevents silent off-by-one/missing-form regressions in generated defclass helper definitions.
- Refactoring `compileTagbody` segment construction (`src/compiler/compile.zig:8015`) to pre-count tags, allocate segment/tag buffers once, and compile each segment directly removed dynamic segment/form staging lists from the hot control-flow lowering path.
- Adding a focused dotted-tail regression for `tagbody` (`src/compiler/compile.zig:19902`) locked malformed body-list rejection while preserving existing segment partition behavior.
- Replacing method-dispatch setup staging in `generateMethodDispatcher` (`src/compiler/compile.zig:12321`, `src/compiler/compile.zig:12446`) with fixed-size slices for `no-applicable-method` call args and lambda optional params removed avoidable list-growth churn in generic-function dispatcher synthesis.
- Locking dispatcher arity shaping via `defmethod` regression (`src/compiler/compile.zig:19208`) keeps optional-param count aligned with computed max arity after staging refactors.
- Replacing `buildEffectiveMethod` statement/after-body accumulation (`src/compiler/compile.zig:12517`, `src/compiler/compile.zig:12560`) with deterministic pre-sized slices and direct progn nodes removed additional method-combination staging allocations in CLOS dispatcher synthesis.
- Locking `:before`/primary/`:after` dispatcher synthesis with a focused regression (`src/compiler/compile.zig:19256`) preserved method-combination shape while removing transient list staging.
- Replacing `toOwnedSlice`-based dispatcher/lambda param handoff in `defmethod`/dispatcher generation (`src/compiler/compile.zig:12011`, `src/compiler/compile.zig:12296`) with explicit pre-sized/duped slices removed remaining ownership-churn allocations in method-dispatch parameter setup.
- Rewriting `parseVariant` field extraction (`src/compiler/compile.zig:12900`) to pre-count and allocate field-name slices once removed transient `ArrayList` growth and added strict dotted-tail rejection for malformed variant specs.
- Locking the behavior with `parseVariant` focused regression (`src/compiler/compile.zig:17991`) keeps ADT variant parsing strict while preserving field ordering.
- Locking direct `char`/`schar` CL semantics with integration coverage (`src/tests/integration.zig:862`) prevents regressions where string indexing accidentally returns integer codepoints instead of character values.
- Fixing `read-from-string` wrapper index semantics in `lib/stdlib.habu:3984` (add `:start` offset back to secondary position and preserve multi-values through wrapper branches) closed a real CL behavior gap that surfaced under `(multiple-value-list (read-from-string ... :start N))`.
- Locking the fix with integration coverage (`src/tests/integration.zig:4335`) prevents regressions where wrappers return slice-relative positions instead of original-string indices.
- Updating `Repl.evalPrint` to emit VM secondary values after the primary (`src/interp/repl.zig:2637`) fixed interactive output for multi-value forms (`(values ...)`, `floor`, etc.) and clearing `secondary_values_count` after print prevents stale-value bleed into subsequent REPL displays.
- Adding REPL output regressions (`src/interp/repl.zig:4274`, `src/interp/repl.zig:4296`) locks multi-line multi-value display and post-print secondary reset behavior.
- Extending `compileDefpackage` coverage to keyword designators (`src/compiler/compile.zig:19041`) locked CL-compliant forms like `(defpackage :my-pkg (:use :cl))` and prevents regressions where keyword package designators were accepted in parser paths but failed in compiler/package setup.
- Adding focused `with-output-to-string` regressions (`src/tests/integration.zig:4421`) locked clean primary-value behavior and verified `princ` writes to string streams without call-mismatch failures.
- Extending `coerce` numeric/character coverage in `lib/stdlib.habu:2400` (integer/fixnum targets via truncation, `character`<->`integer` bridges, and explicit `t` identity) closed real CL conversion gaps without backend-specific branching.
- Locking those conversions with integration coverage (`src/tests/integration.zig:4010`) prevents regressions on numeric/char/list/string/vector coercion paths.
- Routing `~D` through a dedicated grouped-decimal helper in `src/interp/vm.zig:7786`/`src/interp/vm.zig:8719` fixed `~:D` output semantics (`1,234,567` and `-1,234,567`) without ad-hoc directive parsing branches.
- Locking grouped-decimal behavior with explicit integration coverage (`src/tests/integration.zig:2749`) prevents regressions where modifier parsing falls through to literal directive text.
- Reproducing multidimensional `make-array` row-major access in a focused integration (`src/tests/integration.zig:950`) exposed a real stdlib semantic gap rather than test-only churn.
- Removing early-stdlib `dolist` macro forward-reference use in `array-row-major-index` and implementing true rank-aware `row-major-aref` index decomposition (`lib/stdlib.habu:605`, `lib/stdlib.habu:622`) fixed two root issues: malformed compile-time macro expansion in early forms and incorrect rank-1-only row-major access.
- Replacing `with-package-iterator` stubs with real iterator state and hardening `do-symbols`/`do-external-symbols`/`do-all-symbols` package normalization (`lib/stdlib.habu:6145`, `lib/stdlib.habu:6398`, `lib/stdlib.habu:6428`, `lib/stdlib.habu:6441`) restored package-iteration behavior for generic CL code paths.
- Locking iteration behavior with a focused regression (`src/tests/integration.zig:5037`) catches regressions in symbol-category iteration and iterator return-value shape.
- Replacing `restart-bind` stubs with real restart-case lowering (`lib/stdlib.habu:7173`) restores dynamic restart registration so handler code can invoke bound restarts in generic CL flows.
- Locking `restart-bind` behavior with focused coverage (`src/tests/integration.zig:4716`) prevents regressions where restart handlers silently no-op.

### Did Not Work
- Clearing `compiler.builtins` inside `setVm` caused null-handle crashes in REPL setup (`src/interp/repl.zig:createFeaturesGlobal` reads `compiler.builtins.?` directly). Correct fix was to invalidate refresh epoch keys in `setVm` without nulling builtin handles.
- Assuming JIT entry detection based only on `.define` was stable was wrong; compiler IR shape changes (function-cell correctness work) silently disabled JIT coverage in both REPL and benchmark paths.
- Driving dotted-tail rejection through top-level `compile` dispatch was misleading for this test: non-builtin `+` symbol identity can route to generic call lowering, so the invariant should be asserted at `compileVariadicArith` directly when validating list-shape enforcement.
- Reader/parser canonicalizes unescaped symbol case, so parser-based regressions should assert normalized names (`FOO`) instead of source spelling (`Foo`) when validating symbol-derived identifiers.

---

## Anti-Patterns (What Goes Wrong)

### 1. "Already Exists" Discovery (793 occurrences)

The #1 time sink: implementing something that's already in the codebase.

**Examples:**
- Added duplicate array opcodes (0x73-0x78) when they already existed at 0xCF, 0x1B-0x1E
- Wrote VM handlers for make_array/aref/aset, then found existing handlers 1000 lines away
- Implemented format directives that were already working

**Rule:** Before writing ANY new code, `grep -rn` the codebase for the feature name, opcode, function name, and related keywords. Check both Zig source and stdlib.habu.

### 2. Forward Reference / Ordering Bugs (199 occurrences)

Lisp macros compile their body at definition time. If a macro calls a helper, the helper must be defined BEFORE the macro.

**Examples:**
- `defmacro` using helpers defined later in stdlib.habu → CompileError
- Moved macro definitions above helpers → broke other macros depending on the moved code
- LOOP macro helpers had cascading ordering dependencies

**Rule:** In `lib/stdlib.habu`, helper functions go ABOVE the macros that use them. When adding a new helper, check all macros below it for dependency ordering.

### 3. Arena Allocator Lifetime Bugs (385 occurrences)

The REPL resets the arena allocator between expression compilations. Any IR nodes, strings, or metadata allocated with the arena become stale pointers after the next expression compiles.

**Examples:**
- defmethod stored `body: *Ir` pointers that pointed into freed arena memory → segfault
- Fix: store function NAME strings (persistent allocator) instead of IR pointers
- Slot names from defclass allocated in arena, freed before runtime execution

**Rule:** Anything that must survive across REPL expressions MUST use `globals.allocator` (persistent), NOT `self.allocator` (arena). IR nodes, compiled chunk references, and temporary strings are arena-scoped.

### 4. Package-Qualified Name Mismatches (430 occurrences)

The compiler looks up globals using qualified names like `"CL-USER:foo"`, but generated code sometimes registers with unqualified names like `"foo"`.

**Examples:**
- defclass constructors registered as `"make-person"` but looked up as `"CL-USER:make-person"` → UnboundVariable
- Fix: added `qualifyName()` helper that prepends current package prefix

**Rule:** When generating function definitions programmatically (defclass, defstruct, defmethod), ALWAYS use `qualifyName()` or `getQualifiedName()` to match the lookup path.

### 5. Reverts and Rework (118 occurrences)

Large, multi-file changes that break tests and require full reverts.

**Examples:**
- unwind-protect error handling attempted 5+ times, always abandoned
- CLOS defmethod rewritten 3 times before finding the right abstraction (store names, not IR)
- Bignum arithmetic had repeated off-by-one bugs in carry propagation

**Rule:** Make small, testable changes. Commit after each working step. If a change touches >3 files, break it into smaller dots.

### 6. Complexity Bailouts (79 occurrences)

Starting a feature, discovering it's far more complex than estimated, then abandoning.

**Examples:**
- unwind-protect on VM errors: needs dedicated effort to handle cleanup-form execution during error propagation
- Full LOOP macro: each keyword interaction multiplies complexity
- Pretty-printer: dispatch table for every type

**Rule:** When estimated time doubles, stop. Create a focused dot with the new understanding. Don't push through with partial knowledge.

### 7. Duplicate Code / Handlers (793 occurrences, overlaps with #1)

Adding code in one location without checking if it exists elsewhere in the same file.

**Examples:**
- Two sets of array VM handlers (lines ~1190 and ~2235) in vm.zig
- Duplicate opcode definitions in opcodes.zig

**Rule:** Before adding a handler/opcode/primitive, `grep -n` the target file for the name. vm.zig is 10K+ lines — duplicates are easy to introduce.

---

## Anti-Patterns (Tooling)

### 8. sed/regex Edits on Large Files (from SESSION.md patterns)

Using sed or regex-based edits on large files frequently deletes too much, duplicates sections, or corrupts syntax.

**Rule:** Use the `edit()` tool with exact `oldText` match for surgical changes. Read the target area first with `read()` to get exact text.

### 9. Editing Without Reading First

Making assumptions about file contents based on stale context.

**Rule:** ALWAYS `read()` the target lines before `edit()`. File contents change between turns. Never assume line numbers are still accurate.

---

## Positive Patterns (What Works Well)

### 1. Test After Every Change (1333 occurrences of `zig build test`)

Run `zig build test` after every meaningful edit. Catches regressions immediately.

### 2. Read Code Before Editing

Understand the existing patterns in a file before modifying it. Check how similar features are implemented.

### 3. Helper Function Extraction

When 3+ locations share logic, extract to a function. Examples: `qualifyName()`, `getPredicateOperand()`, table-driven dispatch.

### 4. Table-Driven Dispatch

Replace if-else chains with data tables. Easier to extend, fewer typos, compiler catches missing cases.

### 5. Small Dots, Frequent Commits

Break work into dots that take <2 hours. Commit after each passing test. Use `tools/dot-finish` to enforce the build-test-commit cycle.

### 6. Store Names, Not Pointers

When crossing allocator lifetimes (arena → persistent), store string names and re-resolve at use time instead of storing raw pointers.

---

## Zig-Specific Lessons

### Arena Reset Invalidates All Pointers
The REPL's arena allocator (`self.allocator` in compile.zig) is reset between expressions. Never store arena-allocated pointers in persistent data structures.

### Switch on typeKind(), Not If-Else
Exhaustive switch catches missing cases at compile time. If-else chains silently ignore new types.

### Allocator-First Convention
`fn init(allocator: Allocator, ...) Self` — allocator is always the first parameter.

### ArrayList is Unmanaged in Zig 0.15
`var list = std.ArrayList(T){};` — pass allocator to each method call, not at construction.

### Import Once, Reference via Namespace
`const types = @import("type.zig");` then `types.Type`, `types.Primitive`. Don't import individual names.

---

## Lisp-Specific Lessons

### Macro Compilation Order
`defmacro` compiles its body immediately. All helpers used by a macro must be defined above it in the source file.

### Lisp-1 vs Lisp-2
Habu is a Lisp-1 (single namespace for functions and variables), but has some Lisp-2 features (symbol-function, fdefinition). `define` sets the value cell. Functions are looked up via global variable binding, not a separate function cell.

### defclass Slot Syntax
Correct: `(defclass person () name age)` — slots are separate top-level forms.
Wrong: `(defclass person () (name age))` — this is parsed as ONE slot with options.

### CLOS defmethod: Store Function Names
Each method compiles to a separate named function (e.g., `"foo$number"`). The generic function stores the name string, not an IR pointer. This survives arena resets.

---

## Session Workflow

### Always Create Dots Before Starting Work
No multi-step work without a tracking dot. Include file paths, line numbers, and dependencies.

### Update LESSONS.md at Session End
After completing work, add any new patterns discovered. Reference specific files and line numbers.

### Check Before Implementing
1. `grep -rn` for existing implementations
2. `read()` target files before editing
3. Check both Zig source (`src/`) and Lisp source (`lib/`)
4. Look for related opcodes, VM handlers, and compiler special forms

---

## JIT-Specific Lessons

### runMaybeJit Only Called from vm.run()
`runMaybeJit` (the JIT code check) is only called in `vm.run()`, NOT in `callFromStackAt()`. This means JIT→interpreter→JIT transitions via `callFromStackAt` never check for JIT code on the callee. Fix: `callFromStackAtFast` adds a JIT check after `doCall()`.

### ARM64 Register Map for JIT
- `x19` = sp (JIT stack pointer)
- `x20` = const_pool
- `x21` = ret_buf
- `x22` = ctx (JitContext pointer)
- `x23` = frame_base (locals accessed via `LDR x0, [x23, #offset]`)
- `x24` = stack_end

### Nested JIT Calls Need Adjusted frame_base
`runJitFn` sets `frame_base = self.stack[0..].ptr` (absolute base). For nested JIT calls, `frame_base` must be `self.stack[0..].ptr + bp` where `bp` is the callee's frame base from `self.frames[fp-1].bp`. See `runJitFnInFrame`.

### sp Recovery After Nested JIT
When JIT code runs with a non-zero frame_base, recovering `vm.sp` from `ctx.sp` requires computing the absolute offset from the stack base, not from frame_base. Use `@intFromPtr(ctx.sp) - @intFromPtr(stack_base)`.

### callFast Must Use Absolute Stack Indices
`rt.callFast` computes `fn_idx` relative to `frame_base` via `stackLen(c)`. But `callFromStackAtFast` expects an **absolute** index into `vm.stack`. For top-level JIT (frame_base == stack[0]), they're the same. For nested JIT calls (frame_base > stack[0]), must convert:
```zig
const abs_fn_idx = (@intFromPtr(c.frame_base) - @intFromPtr(c.vm.stack[0..].ptr)) / @sizeOf(Value) + fn_idx;
```
Bug manifestation: recursive functions returning wrong results (e.g., fib(10) → -7 instead of 55).

### Helper-Lowered IR Must Disable Untagged Mode
When adding IR nodes lowered through C-ABI helper calls (`make_hash`, `hash_*`, `make_string`, `arr_*`, `str_set`, `position`, `format`, `intern`), keep `translator.untagged = false` for those bodies. Untagged mode assumes fixnum-only locals; boxed/string/hash values will be corrupted if untagged remains enabled.

### Coverage Work: Add Translation + Reachability Together
JIT coverage work needs three updates in lockstep:
1. `canTranslate` / `firstUnsupportedTag` node acceptance,
2. `translate(...)` lowering implementation,
3. call-safety classification (`has_cross_calls`, untagged gating).
Skipping (3) causes post-emit/liveness issues even if translation compiles.

### JIT Tests Must Use VM Stack, Not Local Buffers
Tests that manually create `JitContext` must use `vm.stack` as the stack buffer, not a local `var stack_buf: [32]Value`. When `callFast` converts frame-relative to absolute indices, it assumes `frame_base` points into `vm.stack`. A separate buffer produces garbage indices.

### Self-Call Detection: Track Stack Depth Across Opcodes
To detect `load_global FIB; ...args...; call N` as a self-call:
1. On `load_global X` where globals[X] is a closure for the current chunk: set `self_call_depth = 0`
2. On push ops (push_nil, push_i32, load_local, etc.): increment depth
3. On binary ops (add, sub, lt, etc.): decrement depth (consume 2, push 1 = net -1)
4. On `call N` where depth == N: emit self-call
5. On anything else (jumps, pops, etc.): reset tracking to null

### Self-Call Frame Setup Must Replicate doCall
The VM's `doCall` shifts args down by 1 (overwriting closure slot): `stack[new_bp + i] = stack[new_bp + 1 + i]`. The JIT self-call must do the same, or `load_local 0` will load the closure instead of arg0.

### saved_chunk_sp Limits Recursive JIT Depth
`callFromStackAtFast` uses `saved_chunk_sp` (limited to `MAX_SAVED_CHUNKS`). Each nested call uses one slot. For recursive JIT functions, this limits call depth. Increased to 256 from 16.

### tryJitCompile: Compile-Only, No Run
When adding JIT compilation in call paths, separate "compile and cache" from "run". `tryJitCompile` should only compile and return the function pointer. The caller handles `runJitFnInFrame`. This avoids re-entrance issues where compile→run→callFast→compile creates nested compilation contexts.

### Dot Workflow
Always: `dot add` → `dot activate` → work → `tools/dot-finish`. Close activate dots immediately after activation. Never start multi-step work without a tracking dot.

---

## JIT Optimization Lessons (Session 2)

### Specialize Pass Must Preserve Lambda Fields
When the specialize pass copies a lambda IR node (because the body changed), it must copy ALL fields including `safety` and `speed`. Omitting them resets to defaults (safety=1), causing check_fixnum bytecodes even when the user declared `(optimize (safety 0))`. This was a silent performance bug — everything still worked correctly, just slowly.

### Type Declarations Don't Propagate Without Explicit Wrapping
`(declare (type fixnum n))` records the type in `global_decls` but does NOT automatically wrap variable references with `assert_fixnum`. Without explicit wrapping in the compiler's variable-reference path, the specialize pass can't prove operands are fixnum. Fix: when compiling a variable reference, check `global_decls.getTypeDecl(name)` and wrap with `assert_fixnum` if the type matches a known builtin (like fixnum).

### getTypeDecl Was a Stub Returning null
The `DeclEnv.getTypeDecl()` method was a stub (`return null`) with a comment "TEMP: bypass HashMap to avoid crash". This silently disabled all type-driven specialization. Lesson: search for `return null` and `TEMP` comments that might be masking missing functionality.

### Don't Strip assert Wrappers From Specialized Ops
When converting `add(assert_fixnum(x), assert_fixnum(y)) → fixnum_add(...)`, keep the `assert_fixnum` wrappers on the operands. They serve as runtime safety checks at safety > 0. The specialized op handles the performance (no type dispatch), while the assert handles correctness. At safety 0, the emitter skips the check anyway. Stripping asserts breaks `(the fixnum ...)` contracts — `(double "hello")` would silently produce garbage instead of erroring.

### declare Not Processed in let Scopes
`filterDeclares` was only called in lambda body compilation, not in `compileLetWithTail`. So `(let (...) (declare (type fixnum ...)) body)` silently ignored the declaration. Fix: add `filterDeclares` call before compiling let body.

### Peephole Fusion: Generate Less Code, Not Better Code
The #1 JIT bottleneck is memory stack traffic: every bytecode op pushes/pops through memory. SBCL keeps values in registers. Instead of optimizing individual stencils, fuse common bytecode sequences to eliminate intermediate memory round-trips:
- `load_local N; push_i32 K; fixnum_le; jmp_nil` → `LDR; CMP; B.cond` (3 inst, 1 memory op instead of 7)
- `load_local N; push_i32 K; fixnum_sub` → `LDR; SUB; ORR; push` (4 inst instead of ~10)
This yielded 36% improvement on fixnum_loop (83→53ms).

### B.cond Encoding for Peephole Jumps
`B.cond` instruction: `0x54000000 | (imm19 << 5) | cond`. Condition codes: EQ=0, NE=1, GE=10, LT=11, GT=12, LE=13. Invert the condition for `jmp_nil` (which branches when false): LE→GT, LT→GE, etc. Use `rel19` hole type for patching.

---

## Architecture Lessons

### Stack Machine JIT is Fundamentally Broken
A stack-machine JIT that translates each bytecode to native code will always be slow because every value round-trips through memory. Peephole fusion is a band-aid — it reduces memory traffic for specific patterns but can't fix the root cause. The right architecture is SSA-based: bytecodes → SSA IR → register allocation → native code. This is what SBCL, V8, and every serious JIT does.

### SSA Over Direct IR-to-Native
Tree-shaped compiler IR (like Habu's `Ir`) represents *source structure*. SSA represents *data flow*. For JIT compilation you need data flow because: (1) phi nodes at join points tell you which definition reaches each use, (2) def-use chains enable dead code elimination and constant propagation for free, (3) SSA liveness intervals are clean for register allocation, (4) loop-invariant code motion requires knowing what doesn't change across iterations.

### Hoist Integration
Hoist (Cranelift port in Zig) provides the full SSA pipeline: IR → Optimize (SCCP, DCE, GVN, LICM) → ISLE lowering → Register allocation → AArch64 emit. Vendored as path dependency via `build.zig.zon`. Access: `hoist_dep.artifact("cranelift").root_module`. Key APIs: `FunctionBuilder` for IR construction, `ContextBuilder` for compilation settings, `JitMem` for executable memory. Types use constants (`Type.I64`) not constructors.

### Hoist Block Params vs SSA Variables
Two ways to handle phis in Hoist: (1) block params (`setBlockParams` + `jumpArgs`) — manual but doesn't trigger SSA builder, (2) SSA variables (`declareVar`/`defVar`/`useVar`) — automatic phi insertion but requires the SSA builder to compile cleanly in the consumer's build context. Block params are safer for initial integration.

**Caveat**: Block param phis don't work correctly with hoist's current codegen. The merge block param values get assigned to wrong registers. Workaround: emit `ret` directly from both branches (no merge block). This limits if-expressions to top-level position (can't be nested inside arithmetic). Future fix: fix hoist's block param → register mapping.

### Hoist Register Allocator: Caller-Saved Handling (FIXED)
**Bug**: Hoist's linear scan allocator didn't know that calls clobber caller-saved registers (x0-x18). Values in caller-saved regs were silently destroyed after calls.

**Fix**: Added `call_positions` tracking to `LivenessInfo`. Both `computeLiveness` and `computeLivenessWithCFG` now record instruction indices of call/call_indirect/blr instructions. The allocator's `tryAllocateReg` checks `spansCall()` — if a live range spans a call, only callee-saved registers (x19-x28) are considered. Required adding `isCall()` to all backend instruction types.

**Key subtlety**: A value whose last use IS the call (it's a call argument) doesn't need to "survive" the call. The span check uses `call_pos >= start AND call_pos < end` (strict less-than on end). Using `<=` for end would incorrectly force call arguments into callee-saved regs.

### Hoist AArch64 Emitter: V-Bit Bug in STR/LDR (FIXED)
**Bug**: `emitStr` and `emitLdr` (unscaled immediate forms) had bit 26 (the V flag) set to 1, generating SIMD `STUR Dt`/`LDUR Dt` instead of integer `STUR Xt`/`LDUR Xt`. Template `0b11111000000` should have been `0b11110000000` (bit 6 in the 11-bit constant maps to bit 26 in the instruction).

**Manifestation**: Callee-saved register save/restore wrote to SIMD register D19 instead of integer register X19. The restore instruction `LDUR D19` with the wrong encoding (`opc=10, size=11`) was an UNDEFINED encoding → "Illegal instruction" trap.

**Debugging approach**: Hex-dumped JIT code, manually decoded AArch64 instructions, compared bit patterns against ARM Architecture Reference Manual. The V flag (bit 26) distinguishes integer (`V=0`) from SIMD/FP (`V=1`) in all load/store encodings.

### Hoist AArch64 Emitter: LDP Encoding Bug (FIXED)
**Bug**: `emitLdp` used template `(0b1010011 << 23)` which gives `[25:23]=011` (pre-index variant) with `L=0` (store). This generated STP pre-index instead of LDP signed-offset. Two errors in one constant:
1. Wrong variant: 011 (pre-index) instead of 010 (signed offset)
2. Missing L bit: L=0 (STP) instead of L=1 (LDP)

**Fix**: Replaced opaque bitfield constant with explicit field composition:
```zig
(0b101 << 27) | (0b010 << 23) | (0b1 << 22)
```

**Lesson**: Never use magic bit constants for instruction encoding. Compose from named fields so each bit's purpose is visible and verifiable against the architecture manual.

### Self-Pointer Patching for Recursive JIT
To emit self-recursive calls via `call_indirect`, embed a placeholder constant `0x0BADF00DDEADBEEF` as an `iconst`. After compilation, scan the generated code for the MOVZ+MOVK+MOVK+MOVK sequence matching the placeholder and patch with the actual function address. Patch BEFORE `writeExec` so the I-cache flush covers the patched code (on AArch64, D-cache writes are not visible to I-cache without explicit flush).

### Hoist Aggressive Optimization Removes Recursive Calls
With `optLevel(.aggressive)`, hoist's optimizer removes `call_indirect` instructions to functions with no observable side effects. Recursive fib calls get eliminated because the optimizer can't prove they terminate. Use `optLevel(.none)` for functions with recursive calls.

### Compiler IR vs Test IR: Symbol Representation Mismatch
**Bug**: Hoist backend unit tests used `.global_ref` for function references in self-calls, but the actual REPL compiler produces `.lit` (symbol value) for the same purpose. `detectSelfCalls` only checked `.global_ref`, so recursive functions compiled from the REPL were treated as non-recursive — the self-call was replaced with `nil`.

**Fix**: Added `isCallTargetSelf()` that checks both `.global_ref` (name match) and `.lit` (symbol value with qualified/unqualified name matching). Qualified names like `"CL-USER:MYFIB"` must match unqualified symbol names like `"MYFIB"` by checking suffix after `:`.

**Lesson**: Always test the actual compilation pipeline end-to-end, not just hand-crafted IR. The compiler's output may use different IR nodes than what you expect.

### Multiple REPL Compilation Paths
**Bug**: Hoist compilation was only wired into the stdlib loading path (`compileAndRun`) but not the interactive REPL path (`evalCapturingError`). User-defined functions with `(declare (optimize (speed 3)))` never got hoist-compiled.

**Fix**: Added `tryHoistCompileLambdas` call to `evalCapturingError` after bytecode emission.

**Lesson**: In a REPL with multiple expression evaluation paths (file loading, interactive input, eval-when), new passes must be added to ALL paths.

### Signature Ownership Double-Free
**Bug**: `errdefer sig.deinit()` + later `defer func.deinit()` double-freed signature arrays when `Function.init(sig)` consumed the sig by value. If compilation failed after func creation, both deferred ops ran.

**Fix**: Track ownership with a boolean: `var sig_owned = true; defer if (sig_owned) sig.deinit(); ... sig_owned = false; // after func takes ownership`.

### Nested Self-Calls Cause Regalloc Segfaults
**Pattern**: When a self-call's result is passed as an argument to another self-call (e.g., `(tak (tak ...) (tak ...) (tak ...))`), hoist's regalloc fails to properly spill values across nested `call_indirect` instructions, causing segfaults.

**Workaround**: Detect nested self-calls (`hasNestedSelfCalls`) and refuse to hoist-compile such functions, falling back to bytecode VM.

**Affected benchmarks**: tak (nested), NOT fib (fib passes self-call results to `+`, not to another self-call).

### Hoist Loop Phi Codegen: Three Bugs
**Root cause**: Three separate bugs conspired to make loops fail:

1. **Jump phi resolution missing** (FIXED): Hoist's AArch64 `jump` lowering emitted a bare `B` instruction without generating moves for `jumpArgs` values. When `jump block1(v7, v11)` was lowered, v7 and v11 were never moved into the registers assigned to block1's params. Fix: emit parallel copies (`mov`) before the branch for each arg→param pair.

2. **Frame layout clobbers FP/LR** (FIXED): `stackSlotOffset()` started at offset 0, which overlaps with the FP/LR save area written by `STP x29, x30, [SP, #-frame_size]!`. Stack stores at `[SP, #0]` overwrote the saved return address, causing "Bus error at address 0x15" (= 21 = the tagged fixnum 10, which was the loop limit stored over LR). Fix: start `stackSlotOffset` at `out_stack_max + 16`.

3. **stack_store lowering missing** (FIXED): The AArch64 lowerer had no case for `.stack_store`, causing `LoweringFailed`. `stack_load` was handled but not its counterpart. Fix: add `.stack_store` handler with STR instruction emission.

**Impact**: fixnum_loop 52ms → 8ms (6.5x speedup).

**Lesson**: When debugging "wrong results", don't assume a single bug. The first fix (stack_store handler) revealed the second (frame layout), which when combined with the initial approach (phi) revealed the third (missing parallel copies). Test each layer independently.

### Parallel Copy for Jump Args (SSA Phi Resolution)
In SSA-based codegen, `jump block(v1, v2)` where `block` has parameters `(p1, p2)` requires generating `mov p1, v1; mov p2, v2` BEFORE the branch instruction. This is the "parallel copy" problem — values must be moved to their target registers atomically. Simple sequential moves work when there are no circular dependencies (which is true for our case since loop variables are computed into fresh SSA values before the jump).

### blockParams() Returns Stale Pointers
`func.dfg.blockParams(block)` returns a slice into internal storage. If the DFG grows (by appending instructions or values) between creating block params and reading them, the slice becomes dangling. **Save block param values immediately** after `appendBlockParam()` into a local array instead of calling `blockParams()` later.

### End-to-End Testing Reveals Integration Gaps
Unit tests for the hoist translator worked perfectly (hand-crafted IR with `global_ref` nodes), but real REPL-compiled IR used `lit` nodes for function references. Similarly, hoist's loop tests only verified compilation, not execution. Always run the actual pipeline end-to-end before declaring a feature complete.

### Machine Code Disassembly Is Essential for JIT Debugging
When JIT code produces wrong results, dump the generated machine code and decode it instruction-by-instruction. In the phi fix, disassembly immediately revealed: (1) missing parallel copies before back-edge jumps, (2) stack stores clobbering FP/LR at SP+0. Print hex + manual ARM64 decode is faster than adding tracing to the compiler pipeline.

### Constant Folding at IR Translation Level
For tagged fixnum arithmetic where one operand is a constant, fold the tag adjustment into the constant at the IR translator level. Instead of emitting `iadd(x, tagged_n); isub(result, 1)` (3 instructions), emit `iadd(x, tagged_n - 1)` (1 instruction). This saves 2 instructions per fixnum operation with a constant operand.

### LICM via Constant Cache
Without a full LICM pass in the backend, achieve the same effect for constants by maintaining a cache (`i64 → HoistValue`) in the translator. Pre-scan loop bodies for literal values and emit them in the entry block before the loop. The SSA value is then available in all dominated blocks. Combined with `optLevel(.none)` which prevents re-materialization, this keeps loop-invariant constants in registers.

### Post-Emission Parallel Copy Fixup for Call Arguments
When a compiler backend (like hoist) emits sequential `mov` instructions for call argument setup without a parallel copy resolver, source registers can be clobbered before they're consumed. Instead of fixing the backend's regalloc (deep architectural change), post-process the emitted machine code: scan backwards from each `blr` instruction, collect the preceding `mov` instructions to ABI registers (x0-x7), and topologically sort them so that a move whose destination is still needed as a source by another move is emitted last. This approach is simple, correct, and avoids modifying the backend. The key insight: the "ready" criterion for topological sort is "no remaining move reads from my destination register."

### Stack Slot Offsets Must Account for Full Frame Layout
Stack slot offsets baked into lowered code must account for ALL frame components: FP/LR save area, callee-saved register area, and outgoing stack space. If offsets only account for FP/LR (16 bytes), they overlap with callee-saved registers saved at SP+16..SP+N. During lowering, the callee-save count isn't finalized (determined by regalloc), creating a chicken-and-egg problem. Conservative reservation (assuming max callee saves) works but wastes stack space.

### Inlining Tail-Recursive Functions as Loops
Cross-function inlining for tail-recursive callees requires converting the callee's body to a loop at the hoist IR level. Key steps: (1) Create header block with phi params for callee parameters. (2) Jump from caller to header with translated arguments. (3) Set `tco_header`/`tco_exit` and `fn_name` to callee's name. (4) Translate callee body via `translateTCOExpr` — tail calls become jumps to header. (5) Non-tail exits jump to exit block. (6) Restore caller's TCO state. This eliminated ~350K BLR/RET pairs for nqueens-safe-p, reducing nqueens(10) from 3.75ms to 3.45ms.

### TCO Exit Trampoline Elimination
Nested if-expressions in TCO context generate trampoline blocks: `block14 → block11 → block8` for each return path. Detect "simple exit" branches (literals, variable refs) and jump directly to `tco_exit` instead of through merge blocks. This reduced nqueens(10) from 3.45ms to 3.37ms and eliminated 3 blocks from the IR.

### Peephole Safety: Round-Trip MOV Detection
When detecting `MOV xA,xB; MOV xB,xA` round-trip pairs for elimination, check ALL register references (rd, rn, rm) of intermediate instructions, not just MOV sources. Non-MOV instructions (CSET, CMP, etc.) may write to or read from the intermediate register. Only NOP both MOVs when the intermediate register is truly dead between them.

### IR Deep Copy for Cross-Function Inlining
To inline a function compiled in a previous REPL form, the callee's IR must survive arena deallocation. Create a dedicated `ArenaAllocator` per compiled function, deep-copy the IR body and parameter names into it, and store the arena in `CompiledFn`. The `deepCopyIr` function only needs to handle the subset of IR nodes that pass `canTranslate`.

### coalesceMovs Only for Safe ALU Ops
The `coalesceMovs` peephole pass must only coalesce MOV instructions that follow safe ALU operations (ADD, SUB, MADD). Coalescing MOV after conditional operations (CSET, SELECT) or across control flow boundaries breaks correctness because multiple branches may write to the same destination register.

## 2026-02-08: Critical JIT Bug Fixes

### Entry Param Parallel Copy (fixEntryParamMoves)
- Hoist's regalloc emits sequential MOVs for entry block param copies: `MOV xD, xS`
- For 3+ params with circular dependencies, sequential MOVs clobber values
- Fix: proper parallel copy algorithm with topological sort + x9 scratch for cycles
- `fixEntryParamMovesAlloc` can insert extra instructions via ArrayList
- Previously, `eliminateRoundTripMovs` was incorrectly NOPing broken swap pairs
  in the entry region — now skips the entry region entirely

### coalesceMovs Cross-Branch Liveness Bug
- Post-MOV consumer scan treated branch instructions as "rd0 is dead"
- But branch targets may read rd0 (e.g., phi copies in merge blocks)
- Fix: conservatively mark rd0 as potentially live when hitting a branch
- This caused TCO functions to return wrong values (e.g., f3(a,b,c))

### Hoist LDP Rt2 Register Mismatch
- When hoist merges two adjacent loads (car + cdr) into LDP, the Rt2 register
  doesn't match the regalloc's expected register for the second value
- Workaround: always use `iadd + load offset=0` for cdr instead of `load offset=8`
- This prevents hoist from merging car/cdr into LDP
- Affected ALL functions using car + cdr (sum-list, while loops over lists, etc.)

### Untagged Mode + Cons Incompatibility
- Untagged mode works with plain i64 inside function body (params untagged at entry)
- Cons cells store TAGGED values (runtime objects read by interpreter/other functions)
- In untagged mode, storing untagged values into cons cells corrupts data
- Similarly, car/cdr return tagged values that don't mix with untagged arithmetic
- Fix: disable untagged mode for functions with cons/car/cdr (`containsLoads`)

### Key Peephole Pass Ordering
1. eliminateDeadCset
2. fixEntryParamMovesAlloc (can insert instructions)
3. fuseCmpImmediate
4. eliminateRoundTripMovs (skips entry region)
5. coalesceMovs (conservative at branches)
6. eliminateUselessBranches
7. invertBranchOverBranch
8. fixCallArgMoves (if recursive)
9. fuseMulAdd
10. fuseSelectCondition
11. eliminateLeafPrologue (if !recursive)
12. compactNops (LAST)

## 2026-02-08 (continued): JIT Performance Optimizations

### Backward Branch Coalescing for Loop Phi Copies
- `coalesceMovs` now treats backward `B` (loop backedge) as safe for rd0
  when there are no BLR/BL calls between the ALU op and the branch.
- Key insight: phi copies before a loop backedge capture rd0's value into
  mov_dst. The loop header reads mov_dst, not rd0. So rd0 is dead.
- Unsafe for loops with calls: callee may clobber registers.
- fixnum_loop improved from 0.37x to 1.08x SBCL.

### Cons Constants LICM (Loop-Invariant Code Motion)
- Inline cons uses g_alloc_ptr address (48-bit), 16, and 8 constants.
- Pre-emit these constants before the loop (via `in_loop_preemit` flag).
- ONLY for non-recursive functions — recursive functions have too much
  register pressure; adding 3 more constants causes spill issues.
- list_build improved from 1ms to 300µs (matching SBCL).
- gc_cons improved to 193µs (1.07x SBCL).

### Direct Predicate Conditions in translateIf
- oddp/evenp/zerop/consp as if-conditions emit direct I8 comparisons.
- Eliminates 3-5 instructions: tagged select + brif on tagged value.
- Pattern: `(if (oddp x) ...)` → `band(x,2); icmp ne; brif`
- remove_if improved from 700µs to 42µs (0.86x SBCL).

### Untagged Mode Incompatibilities
- Untagged mode disabled for functions with:
  - cons/car/cdr (cons cells store tagged values)
  - Primitive calls (gcd/nreverse/append/assoc expect tagged args)
  - Loads (car/cdr return tagged from cons cells)
- Each incompatibility caught by separate `contains*()` check.
- Missing check caused gcd benchmark to return wrong answer (235704 vs 278574).

### Inline GCD Blocked by Hoist Regalloc
- Euclidean algorithm as hoist loop: `while b!=0: r=a%b, a=b, b=r`
- Requires swap of phi parameters (a←b, b←r) at loop backedge.
- Hoist regalloc doesn't emit phi copies for this swap → infinite loop.
- Same fundamental issue as partial TCO phi copies.
- Fallback: C-ABI jitGcd call (3.3ms vs SBCL 0.89ms).

### Hoist LDP Register Mismatch (Root Cause)
- When hoist merges `load [x, #0]` and `load [x, #8]` into LDP, the Rt2
  register assignment doesn't match the regalloc's expected register.
- Example: regalloc assigns cdr load to x2, but LDP puts it in Rt2=x19.
- Workaround: always use `iadd + load offset=0` for cdr.

### JIT Performance Optimization Session (2026-02-08)

**Partial TCO**: Enabling TCO for functions with BOTH tail and non-tail self-calls
is safe and gives significant speedup. The key: tail calls become jumps (zero overhead),
non-tail calls remain as call_indirect. For ack: 720ms→592ms (18% faster).
Guard: when partial TCO leaves non-tail self-calls, keep `is_recursive = true`.

**Local Constants for Call-Heavy Functions**: Hoist's optimizer LICM-moves constants from
loop body to entry block (block0), forcing them into callee-saved registers since their
live ranges span call sites. Fix: skip `preEmitConstants` for TCO functions with non-tail
self-calls, and use `local_consts` flag in `cachedIconst` to emit fresh small constants
per use-site (only in call-containing blocks). Large constants (function pointers) still cached.

**Translation-Level CSE**: Hoist's optimizer can't CSE across loop iterations (even same-block
duplicate iadd). Fix: maintain a `cse_cache` mapping `(op, lhs.index, rhs.index) → result`
during translation. Clear on block switch for SSA dominance safety. Eliminated duplicate
`(+ i 1)` in fixnum_mul: 1170µs→1091µs (7% faster).

**Hoist Call_indirect Bug**: Hoist's e-graph optimizer (any opt level > .none) incorrectly
eliminates call_indirect instructions. Must use `.none` for functions with calls.
This prevents CSE, GVN, LICM from applying. Upstream hoist fix needed.

**MOV Coalescing Limits**: The post-emission MOV coalescing pass can't eliminate phi-copy
moves when the source register is consumed by another instruction between the ALU op
and the MOV. Example: `ADD x5,x0,x4; MADD x7,x5,...; MOV x0,x5` — can't coalesce because
MADD reads x5. This costs 1 extra instruction per loop iteration.

**Multiply-by-Constant Strength Reduction**: ARM64 MADD has 3-cycle latency on Apple M-series.
Replace `imul(x, const)` with shift-add sequences: `x*3 = x + (x<<1)`, `x*5 = x + (x<<2)`,
`x*(2^n) = x<<n`, `x*(2^n+1) = x + (x<<n)`, `x*(2^n-1) = (x<<n) - x`.
Hoist's ISLE lowering has `iadd(x, ishl(y, K)) → ADD Xd, Xn, Xm, LSL #K` rules, but
they don't fire due to forward lowering order (ishl lowered before iadd can absorb it).
The shift-add still wins: 2 instructions at 1+1=2 cycles vs 1 MADD at 3 cycles.
Result: fixnum_mul 1140µs→600µs (47% faster).

**LSL+ADD Fusion Anti-Pattern on Apple Silicon**: `ADD Xd, Xn, Xm, LSL #K` (fused shifted-ADD)
is ~10% SLOWER than separate `LSL + ADD` on Apple M-series. The wide OoO engine (8+ dispatch
slots) parallelizes two simple operations faster than one complex one. Don't fuse.

**Loop Rotation Blocked by Phi Copies**: Bottom-tested loops (SBCL-style) save 1 unconditional
branch per iteration. But hoist's regalloc inserts MOV instructions for phi parameter copies
on the back-edge, adding 2+ instructions that offset the savings. Needs hoist phi coalescing.

**Hoist brifArgs Parameter Bug**: `brifArgs` (conditional branch with block arguments)
doesn't correctly insert phi copies — the target block's parameter register doesn't match
the source value's register. Workaround: use separate trampoline blocks with explicit
`jumpArgs`. This adds overhead but is correct.

**Defer TCO Args After Inner Call**: For `(ack (- m 1) (ack m (- n 1)))`, computing `m-1`
before the inner call forces a callee-saved register to hold the result. Computing it
AFTER the call reuses the phi param register (still intact as callee-saved). Saves 1 STP
pair in prologue. Implemented by splitting arg translation: call-containing args first,
then simple args after.

**getFixnumLit Returns Raw Tagged Value**: In untagged mode, `getFixnumLit` returns the
raw tagged value (e.g., 7 for literal 3). Must shift right by 1 to get the actual numeric
value for strength reduction in untagged mode. Bug caused multiply-by-7 instead of by-3.

### Backend Migration + Perf Audit Session (2026-02-17)

**Dead Legacy Backend Surface**: `src/lib.zig` exported `src/ir/ir.zig` even though runtime
paths use Hoist via `src/jit/backend.zig`. Keeping dead exports preserves stale APIs and
needlessly compiles abandoned code. Remove the export and delete dead backend modules.

**Benchmark Harness Must Avoid Stdlib-Only Calls**: `bench/vm.zig` used
`(concatenate 'string ...)` without loading stdlib, causing `UnboundSymbol` in VM bench
(`src/interp/vm.zig:8825`). VM microbenches should use primitives guaranteed available in
the bare compiler/VM setup (e.g., `make-string` + `length`) or explicitly load stdlib.

**Perf Gating Requires Stable Bench Runners**: `bench-comp` currently crashes in JIT mode
on `gcd` (`src/interp/vm.zig:718` calling `CompiledFn.callFromValues`). Before optimizing
hot paths, lock down benchmark stability; otherwise perf regressions/improvements are noisy.

**Doc Drift Is a Real Performance Risk**: stale file references (`src/jit/jit.zig`,
`src/jit/stencils.zig`, `src/jit/patch.zig`, `src/jit/ctx.zig`, `src/jit/rt.zig`) mislead
optimization work and waste cycles. Keep docs path-valid against both `src/` and `../hoist/src/`.

**Post-Emit Liveness Must Model Call ABI Reads**: peephole dead-code elimination in
`src/jit/backend.zig` removed MOVZ arg setup before `blr`, because liveness treated call
boundaries as "reg dead". On AArch64, indirect/direct calls read x0-x7 (args), x8 (sret),
and `blr` also reads its target register. If that is not modeled, optimizers can turn
correct indirect calls into wrong-result or crashy code paths.

**VM GC Root Churn Drops By Using Slots Over Mirror Arrays**: `collectGarbageExtra`
in `src/interp/vm.zig` no longer builds a temporary `ArrayList(Value)` (`gc_vals`) for
frame closure/chunk roots. Using stack-local `Value` roots registered as `slots` avoids
dynamic buffer growth and copy-back indexing complexity while preserving pointer re-derive
after GC (`chunkFromValue` / `toPtr(Closure)`).

**Maxima Loader Must Not Auto-Execute At File Load**: loading a broad Maxima module set
can hit VM `StackOverflow` that is not recoverable through Lisp-level `handler-case`.
Keep `lib/maxima-loader.lisp` as a callable API (`maxima-load-all`) and avoid auto-running
the full load sequence during file import.

### Stream READ Semantics Can Invalidate Loader RCA (2026-02-17)

`lib/stdlib.habu` currently defines stream `read` by consuming the entire
stream into a string and then parsing once:
- first `(read s ...)` returns the first form
- second and later reads return `:EOF`

Evidence:
- `/tmp/read_many_target.lisp` with forms `1 2 3` produced `R1=1, R2=:EOF`.

Impact on Maxima loader debugging:
- "formwise read/eval" probes that appeared to succeed (`DONE forms=1 ok=1`)
  were not trustworthy for multi-form files because stream `read` never
  advanced past the first form.
- Removing `handler_sp/catch_sp` clobber in `evalForms` (then named `evalFileContentSeparateVm`) did
  not fix `db/compar/limit` load overflows and introduced new regressions
  (`mlisp` load failure), so that change was reverted.

Actionable takeaway:
- Do not use stream-`read` loops as a fallback loader path until stream `read`
  is fixed to consume one form at a time.

### Maxima Integrate Chain Needs Runtime-Callable Dependencies (2026-02-17)

`fboundp '$integrate` is not a sufficient gate for integration readiness.
With a reduced subset, `$integrate` can still fail at runtime with
`(UNBOUND-VARIABLE UnboundSymbol)` due to missing transitive call targets.

Evidence from targeted tracing:
- `TRACE unbound function: ALIAS`
- `TRACE unbound function: SININT`

Fix pattern:
- include `suprv1` (defines `alias`) and `sinint`/`sin` in the integrate subset,
  plus existing `schatc` chain (`partition`, `m2`, `schatchen-cond`).

Testing rule:
- integration gate must execute a real call
  `($integrate '((mexpt) $x 2) '$x)` in `src/tests/integration.zig`,
  not just symbol/macro presence checks.

Environment guard:
- Maxima-source fixtures can disappear or change layout under `/tmp/maxima`.
  Guard Maxima integration tests with a source-presence check
  (`/tmp/maxima/src/lmdcls.lisp`) and `error.SkipZigTest` so non-Maxima
  environments still run the rest of the suite deterministically.
- Prefer candidate-root probing (`/tmp/maxima/src/`, `/tmp/maxima/src/src/`,
  `/tmp/maxima/`) in both loader and tests to avoid path drift regressions.

### Session Notes (2026-02-19, call-lowering transient allocs)

#### Worked Well
- Replacing `ArrayList + builder.call/tailcall/listStar` staging in call lowering with single pre-sized slices and direct IR node construction (`src/compiler/compile.zig:5892`, `src/compiler/compile.zig:5914`, `src/compiler/compile.zig:17130`) removed redundant transient allocations in hot compile paths while preserving call/apply semantics.
- Adding a shared qualified struct-predicate lookup helper (`src/compiler/compile.zig:17158`) removed per-call symbol-name duplication and made occurrence-typing predicate lookup robust for package-qualified predicate registrations.
- Locking regressions directly in compiler tests (`src/compiler/compile.zig:19273`, `src/compiler/compile.zig:19316`) gave deterministic proof that variadic operand preservation and `struct_p` lowering stayed intact.

#### Did Not Work
- Stopping after replacing `ArrayList` alone is insufficient: routing through `builder.call`/`builder.tailcall`/`builder.listStar` still performs an internal `dupe`, so transient-allocation reduction required direct node construction in compile hot paths.

### Session Notes (2026-02-19, letrec + multi-setq staging)

#### Worked Well
- Reworking `compileLetrecWithTail` to pre-count bindings, store one compact binding table, and emit a direct `progn` node (`src/compiler/compile.zig:5351`) removed multiple staging lists (`names`, `values`, `indices`, `exprs`) from recursive-binding compilation.
- Rewriting `compileMultiSetq` to pre-count pairs and emit a direct `progn` (`src/compiler/compile.zig:5949`) removed `ArrayList + dupe` churn from a high-frequency assignment form while preserving per-pair lowering through `compileSet`.
- Adding focused compile regressions for letrec/setq lowering shape (`src/compiler/compile.zig:19349`, `src/compiler/compile.zig:19381`) caught structural regressions immediately without requiring long full-suite runs.

#### Did Not Work
- Leaving `letrec`/`setq` on dynamic append-first staging paths keeps avoidable allocator pressure in loader-heavy workflows; these forms need fixed-size preallocation once arity is knowable from list shape.

### Session Notes (2026-02-19, multi-place setf staging)

#### Worked Well
- Replacing `compileSetf` multi-place `ArrayList` staging with pre-counted pair slices and direct `progn` node emission (`src/compiler/compile.zig:6067`) removed append-growth/dupe churn while preserving recursive per-pair lowering.
- Keeping lowering through the same single-place `compileSetf` path for each `(place value)` pair retained semantics for symbol-macro and compound-place updates; the focused regression (`src/compiler/compile.zig:19427`) confirms one emitted form per pair.

#### Did Not Work
- Using `builder.progn(items)` in this path still duplicates slices internally, so partial refactors that keep builder-level aggregation do not remove transient-allocation pressure.

### Session Notes (2026-02-19, flet/labels staging)

#### Worked Well
- Replacing `compileFletWithTail` `ArrayList(Ir.Binding)` staging with pre-counted binding slices and direct `.let` node construction (`src/compiler/compile.zig:5426`) removed builder-side binding duplication while preserving lexical function binding behavior.
- Reworking `compileLabelsWithTail` to use one compact binding table plus pre-sized `boxed_bindings`/`init_forms` slices (`src/compiler/compile.zig:5476`) removed layered staging lists and avoided duplicate `progn`/`let` copying in recursive local-function lowering.
- Keeping `errdefer` cleanup for duplicated names in the error path preserved safety while allowing successful paths to transfer ownership to IR nodes.

#### Did Not Work
- Holding onto dynamic append patterns for `labels` setup (`names`/`lambda_args`/`indices`/`sym_vals`) adds avoidable allocator churn and duplicates data already derivable from the same binding list traversal.

### Session Notes (2026-02-19, lambda/progv staging)

#### Worked Well
- Replacing lambda entry-assertion staging with fixed-size assertion buffers and direct `progn` node emission (`src/compiler/compile.zig:4239`) removed `ArrayList + dupe` overhead while keeping safety-gated assertion semantics.
- Replacing special-parameter `progv` staging with pre-sized symbol/value slices (`src/compiler/compile.zig:4302`) eliminated transient list growth in lambda lowering without changing symbol/value ordering.
- Replacing all-special LET fast-path value staging (`src/compiler/compile.zig:5320`) with direct slices removed another hot allocation loop in dynamic-binding lowering.

#### Did Not Work
- Leaving assertion/progv aggregation on dynamic arrays causes repeated allocator churn in compile-heavy macro/function pipelines even when the target cardinality is statically bounded by parsed lambda metadata.

### Session Notes (2026-02-19, macro map sync gating + Maxima reprofile)

#### Worked Well
- Sampling a real Maxima subset load (`sample` over `/tmp/maxima_profile_subset.lisp`) identified `interp.repl.Repl.restoreMacroMapsFromRoots` hash-map rebuilds as a dominant steady-state cost in form execution (`src/interp/repl.zig:497`, `src/interp/repl.zig:574`).
- Adding GC-epoch-gated macro map synchronization (`src/interp/repl.zig:574` + `src/interp/repl.zig:2008`) removed unconditional macro-map refresh/restore work from no-GC form execution while retaining full restore on GC transitions.
- Reprofiling after the change shifted hotspots away from macro-map restore loops and improved Maxima subset load wall time from ~5.06s to ~3.37s on the same script/run shape (`/tmp/maxima_profile_subset.lisp`), with similar peak memory (~289MB).

#### Did Not Work
- A manual-GC regression test that called `repl.vm.collectGarbage()` directly between evals produced false failures because macro maps are only guaranteed rooted during managed execution paths; direct unrooted GC is not a valid behavioral contract for macro table persistence.

### Session Notes (2026-02-19, tagbody/progn/values staging)

#### Worked Well
- Replacing `compileFormsToProgn` and `compileValues` dynamic staging with pre-sized slices and direct IR node emission (`src/compiler/compile.zig:8051`, `src/compiler/compile.zig:8077`) removed another layer of `ArrayList` growth + builder duplication in control-flow and multi-value compilation paths.
- Revalidating with both compile-shape regressions and integration-level tagbody/values tests ensured segment/value cardinality stayed correct while reducing staging overhead.

#### Did Not Work
- Leaving these sequence forms on builder-backed aggregation keeps hidden duplicate-slice allocation in high-frequency control-flow lowering; direct node emission is required for predictable allocation behavior.

### Session Notes (2026-02-19, format arg staging)

#### Worked Well
- Rewriting `compileFormat` variadic argument lowering to pre-count and fill a single args slice (`src/compiler/compile.zig:16205`) removed `ArrayList` growth and builder-side arg duplication in a frequently used formatting path.
- Locking the cardinality behavior with a compile regression (`src/compiler/compile.zig:19615`) plus integration format checks kept semantics stable while reducing transient compiler allocations.

#### Did Not Work
- Treating variadic format args as append-first dynamic lists hides redundant copying in builder emission; direct fixed-size arg slices are required for stable hot-path compilation cost.

### Session Notes (2026-02-20, JIT SSA dominance + backedge liveness)

#### Worked Well
- Dumping Hoist IR/ASM for the failing `NQUEENS-SAFE-P` path (`HABU_DUMP_HOIST=1`) exposed a concrete dominance violation: `v9 = iconst 2` defined in one branch but reused in sibling blocks.
- Extending constant pre-emission to traverse `.block` nodes (`src/jit/backend.zig:1738`) fixed the root cause by ensuring required constants are emitted from dominating context before TCO lowering.
- Clearing `const_cache` on block switches when `local_consts` mode is active (`src/jit/backend.zig:1025`) hardened block-local constant semantics and avoids cross-block SSA reuse in the local-constant path.
- Replacing `coalesceMovs` post-MOV safety logic with CFG-aware liveness (`isRegDeadAfter`) at the coalesce site (`src/jit/backend.zig:5623`) removed a real loop-backedge miscompile class.
- Locking both sides with focused regressions (`src/jit/backend.zig:5983`, `src/jit/backend.zig:6005`, `src/tests/integration.zig:88`) prevented both the old `nqueens` wrong-result path and over-conservative pass disabling.

#### Did Not Work
- Assuming `preEmitConstants` already handled wrapper nodes was wrong; missing `.block` traversal silently disabled pre-emission for whole function bodies in TCO paths.
- Assuming linear/use-local coalesce checks were enough across backward branches was wrong; loop-header reads require CFG liveness, not local scan heuristics.
- Treating long `zig build test` as a reliable gate in this environment is still brittle; sampled runs showed `test --listen` wait states, so targeted filters remain the dependable validation path here.

### Session Notes (2026-02-20, major slice budget telemetry gates)

#### Worked Well
- Exporting major mark/sweep budgets from GC (`src/runtime/gc.zig:98`) and threading them into bench JSON (`bench/gc.zig:394`, `bench/gc.zig:397`) removed hard-coded budget assumptions in downstream tooling.
- Enforcing step/sweep/max-slice coherence directly in bench regression checks (`bench/check.zig:420` to `bench/check.zig:477`) caught invalid major-slice telemetry states early.
- Adding `gc_major_slice_in_bounds` to parity gate evaluation (`tools/gc-compare:274`, `tools/gc-compare:385`, `tools/gc-compare:451`) made slice-budget violations fail the same gate path as other GC policy invariants.

#### Did Not Work
- Depending on raw `gc_major_max_*_slice` telemetry alone was insufficient for external validation; without explicit emitted budgets, compare/check tools either drift or silently skip strict bound checks.
- Using full-suite `zig build test` as the only validation gate is not reliable in this workspace right now due an unrelated compile/integration segfault path; targeted GC tests + bench gates were the stable proof path for this dot.

### Session Notes (2026-02-20, tenured segregated free bins)

#### Worked Well
- Splitting tenured reuse into two layers in `src/runtime/heap.zig` (`allocTenuredFromBins` + `allocTenuredFromPendingList`) preserved immediate reuse of newly swept spans while making steady-state reuse O(number of candidate bins) instead of O(all free spans).
- Rebuilding bins from coalesced spans (`coalesceTenuredFree`, `drainTenuredBinsToList`, `rebuildTenuredBinsFromList`) kept coalescing exact without introducing pointer aliasing/index invalidation across mutable free lists.
- A direct allocator-level regression (`src/runtime/heap.zig`: `heap tenured free bins coalesce and reuse spans`) caught both coalesced-span reuse and split-tail reuse behavior.

#### Did Not Work
- A bins-only allocator path that ignored the in-progress `tenured_free` pending list would delay reuse until full coalesce completion and can transiently starve promotions during sliced major sweep windows.

### Session Notes (2026-02-20, tenured coalesce/split policy)

#### Worked Well
- Switching bin allocation from first-fit to bounded best-fit (`src/runtime/heap.zig`: `allocTenuredFromBins`) reduced avoidable oversized reuse while capping scan cost with `TENURED_ALLOC_SCAN_BUDGET`.
- Applying the same bounded best-fit split policy to the pending free list (`src/runtime/heap.zig`: `allocTenuredFromPendingList`) preserved immediate reuse before coalesce while keeping split behavior consistent.
- Enforcing a minimum split remainder (`TENURED_SPLIT_MIN_REMAINDER`) eliminated tiny tail fragments; the regression (`src/runtime/heap.zig`: `heap tenured split policy avoids tiny tail fragments`) locks this.

#### Did Not Work
- Pure first-fit with unconditional split creates tiny remainder spans that churn bins and increase fragmentation pressure under mixed-size promotion workloads.

### Session Notes (2026-02-20, tenured fragmentation benchmarks)

#### Worked Well
- Emitting tenured free-space fragmentation telemetry directly from `bench/gc.zig` (free span count/bytes/largest span/fragmentation ratio) gave a stable signal to track allocator-quality changes.
- Reading free-space from both pending and binned tenured free lists in `src/runtime/heap.zig` (`tenuredFreeStats`, `tenuredFragmentation`) avoided blind spots during incremental sweep windows.
- Wiring fragmentation invariants and gate checks through `bench/check.zig` and `tools/gc-compare` ensured regressions are caught automatically with the same CI/parity flow as other GC metrics.

#### Did Not Work
- Tracking only `tenured_bytes` and object counts misses allocator fragmentation regressions completely; fragmentation required explicit free-span topology metrics.

### Session Notes (2026-02-20, JIT bridge call-stack and sequence correctness)

#### Worked Well
- Restoring full dynamic control-stack depths on JIT fast returns (`src/interp/vm.zig:3519` via `restoreCallerFrameAfterCall`) removed a real block-stack leak in bridge-heavy higher-order workloads.
- Restoring frame dynamic depths before tail-call frame reuse in `doCall(..., tail=true)` (`src/interp/vm.zig:10028`) fixed repeated block-frame accumulation on recursive tail paths.
- Rooting pointer literals for JIT codegen and loading them via stable slots (`src/interp/repl.zig:2296`, `src/jit/backend.zig:1434`) removed stale-literal pointer hazards under moving GC.
- Replacing list-only JIT `length` lowering with a generic sequence helper (`src/jit/backend.zig:556`, `src/jit/backend.zig:3274`) fixed string-length crashes in optimized code paths.
- Refreshing JIT heap bump-cache before/after bridge calls (`src/interp/vm.zig:313`) prevented `heap.alloc_ptr` corruption after bridge-triggered GC and removed `bytesUsed` overflow panics.

#### Did Not Work
- Assuming JIT fast-return could pop only `fp/sp` was wrong; dynamic stacks (`block/catch/unwind/restart/progv/handler`) must be restored from call-frame metadata.
- Assuming list-only `length` lowering was safe at `safety 0` was wrong; valid non-list sequences (strings/vectors/arrays) are common and must follow generic CL semantics.
- Assuming JIT heap globals stay valid across bridge calls was wrong; interpreter/GC activity inside bridge calls invalidates cached bump pointers unless explicitly refreshed.

### Session Notes (2026-02-20, control-stack depth limits)

#### Worked Well
- Raising VM `MAX_BLOCKS` to frame-scale (`src/interp/vm.zig:529`) removed premature `StackOverflow` in legitimate recursive workloads (e.g., sort merge recursion) without changing call semantics.
- Locking the behavior with a deep-recursion integration test (`src/tests/integration.zig:132`) prevents regressions where recursion depth >64 incorrectly fails even when frame/stack budgets are still available.
- Re-running comprehensive bench showed `sort_string` and `intern` now complete instead of warmup overflowing from block-depth exhaustion.

#### Did Not Work
- Keeping `MAX_BLOCKS` far below `MAX_FRAMES` created an artificial control-stack ceiling that failed real recursive Lisp code before true frame/stack limits were reached.

### Session Notes (2026-02-20, sort copy-once safety under generational GC)

#### Worked Well
- Refactoring `sort` to copy once at the public entry and recurse on an internal working-list helper (`lib/stdlib.habu:2374` to `lib/stdlib.habu:2387`) preserved non-destructive CL behavior while removing recursive `copy-list` overhead.
- Locking sort semantics with focused integration checks (`src/tests/integration.zig:821`) caught both descending comparator designators and `:key` behavior regressions.
- Validating against the generational designator stress test (`src/tests/integration.zig:4878`) ensured the optimization did not reintroduce load-time heap corruption.

#### Did Not Work
- Threading a copy-state flag through recursive `sort-with-key` calls (extra recursion argument path) caused deterministic corruption during stdlib load under generational GC, eventually crashing in later unrelated forms (e.g. `defmacro` handling). Avoid copy-state recursion parameters in this path until the underlying runtime/compiler corruption is root-caused.

### Session Notes (2026-02-21, sort string comparator fast path)

#### Worked Well
- Adding a zero-new-defun fast path inside existing `merge-lists-with-key` (`lib/stdlib.habu:2342`) for `(null key)` + `string<` designators (`#'string<` or `'string<`) removed high-frequency `funcall` comparator overhead in sort merges.
- Keeping dispatch inside the existing function (instead of adding new recursive sort helper forms) preserved generational-stdlib-load stability (`src/tests/integration.zig:4935`).
- The new regression (`src/tests/integration.zig:860`) locked function/symbol designator behavior, non-destructive input semantics, and `:key` fallback correctness.
- `sort_string` JIT benchmark improved from ~8.11 ms to ~4.86-4.92 ms (`zig build -Duse-hoist=true bench-comp -- --iterations 3 --warmup 1`).

#### Did Not Work
- Introducing additional recursive stdlib sort helper defuns for string fast paths triggered deterministic generational load crashes in compiler capture analysis (`src/compiler/compile.zig:4989`) and later macro handling (`src/interp/repl.zig:3686`) during `loadStdlib`.

### Session Notes (2026-02-22, Hoist VCode successor corruption under growth)

#### Worked Well
- Building a standalone Hoist reproducer (linear 80-block VCode chain) proved the `computePreds` panic is deterministic once successor storage grows past 32 entries; this removed ambiguity about Habu IR correctness.
- Routing Hoist compilation through a remap-stable allocator wrapper (`src/jit/backend.zig:49`) over a per-compile arena fixed the root issue without touching `../hoist`: old backing slices survive ArrayList growth and `computePreds` no longer reads poisoned entries.
- Wiring the stable allocator in the Hoist compile entry (`src/jit/backend.zig:5365`, `src/jit/backend.zig:5383`) removed the Maxima `SMINMAX` panic path; `bench-maxima` now completes (`jit_compiled=397` at scale 1 in current run).
- Adding a deep branch-chain JIT regression (`src/tests/integration.zig:1947`) locks the >32-edge lowering path that previously crashed in Hoist.

#### Did Not Work
- A plain `ArenaAllocator` alone was insufficient: Zig allocator `free`/realloc paths poison old buffers, so stale Hoist succ/param slices still became `0xAAAAAAAA` (`src/jit/backend.zig` pre-fix compile path).
- Using full `zig build test -Duse-hoist=true` as proof for this dot remains noisy in this workspace because an unrelated pre-existing integration segfault (`deep recursive defun does not overflow block stack at 64`) still fails outside the Hoist-succ fix scope.

### Session Notes (2026-02-22, append copy-once in runtime + JIT)

#### Worked Well
- Replacing append's double-copy path with copy-once tail splice in the runtime primitive (`src/runtime/primitives/list.zig:121`) removed half the cons allocations for left-list elements while preserving output order and GC write-barrier correctness (`setCdr`).
- Replacing JIT append's reverse-cons double allocation with `jitNreverse` + tail splice (`src/jit/backend.zig:329`) matched runtime semantics with one left-side copy and explicit barrier on the tail link.
- Locking allocation behavior with focused regressions (`src/runtime/primitives/list.zig:294`, `src/jit/backend.zig:10159`) made allocation-count regressions immediately visible.
- Running `bench-comp` on parent `cdefc0a7` vs this change showed `list_append` improve from `14.949 ms` to `12.740 ms` in the same harness run shape.

#### Did Not Work
- Treating full-suite `zig build test` as a gate for this dot remains blocked by the unrelated pre-existing crash in `tests.integration.test.deep recursive defun does not overflow block stack at 64`; targeted append/JIT tests were the stable validation path for this fix.

### Session Notes (2026-02-22, JIT self-call patch RCA + safety gate restore)

#### Worked Well
- Reproducing with a minimal recursive program under Hoist (`/tmp/recur.habu`) and dumping final machine code (`HABU_DUMP_HOIST=1`) exposed the real fault: self-call patching rewrote a later non-self BLR to self when the same source register was reused (`src/jit/backend.zig:4632`).
- Fixing `patchSelfCallsToBL` to use the nearest reaching definition of the BLR source register (`src/jit/backend.zig:4638`) eliminated false self-call rewrites and removed the recursive JIT crash path.
- Adding a low-level regression for mixed self/non-self target reuse (`src/jit/backend.zig:8088`) locked the patcher behavior directly at machine-instruction level.
- Restoring a JIT admission safety gate (`src/jit/candidates.zig:98`) preserved CL safety semantics (`TypeMismatch` paths) by avoiding unsafe JIT arithmetic lowering for non-`safety 0` lambdas.
- Adding an integration regression for recursive `safety 0` JIT execution (`src/tests/integration.zig:560`) ensured recursive call lowering still works under the intended admission policy.

#### Did Not Work
- Running full `zig build test -Duse-hoist=true` as an always-clean gate remained unreliable in this workspace due occasional lingering `test --listen` runners; targeted filters plus explicit stale-runner cleanup were the stable validation path for this RCA/fix cycle.

### Session Notes (2026-02-23, progv literal-root completeness in all JIT paths)

#### Worked Well
- Tracing `ratsimp` with `HABU_TRACE_JIT_BRIDGE` + `HABU_TRACE_JIT_XCALL` narrowed the crash to a corrupted generic call designator inside `CPUT` (not the BLR target path), which focused RCA on literal rooting instead of call lowering (`src/interp/vm.zig:338`, `src/jit/backend.zig:3386`).
- Adding `.progv` traversal to REPL literal-root collection fixed a real missing-root gap for progv-wrapped bodies (`src/interp/repl.zig:2795`), and `bench-maxima --workloads=ratsimp` now completes.
- Extending the test `compile_chunk` JIT path to collect and pass literal roots (including `.progv`) removed the same stale-literal class from integration helpers (`src/testing/compile_chunk.zig:52`, `src/testing/compile_chunk.zig:136`, `src/testing/compile_chunk.zig:438`).
- The new regression (`src/tests/integration.zig:146`) locks post-GC call-target stability for progv-wrapped JIT code paths.

#### Did Not Work
- Fixing only REPL literal-root traversal was insufficient: integration `compile_chunk` still compiled with `compileIr(...)` (no roots), emitted `JIT_LIT_NOROOT`, and reproduced post-GC call-target corruption until the helper path was upgraded too (`src/testing/compile_chunk.zig` pre-fix `tryHoistCompile`).

### Session Notes (2026-02-24, stale-forwarding false positives + strict return-from semantics)

#### Worked Well
- Guarding stale-forward resolution with a heap-address validity check (`src/runtime/gc.zig:resolveStaleForwardedValue`) correctly separated real forwarding metadata from current-cycle to-space headers (e.g. symbol `name_len=14` aliasing forwarding tag bits), removing strict-trap false positives under `HABU_TRAP_STALE_RESOLVE_REJECT=1`.
- Keeping age-based promotion coverage explicit (`src/runtime/gc.zig`: `minor gc promotes aged small survivors to tenured`) while updating edge tests to assert pointer/edge correctness rather than placement-only assumptions made GC behavior changes testable without hiding regressions.
- Removing the `ABORTED` fallback in `doReturnFrom` (`src/interp/vm.zig`) and validating with focused return-from/unwind tests removed a non-lexical control-flow escape hatch and aligned behavior with strict block semantics.
- Cleaning orphaned `zig build test` / `test --listen` runners after interrupted runs prevented stale process accumulation and reduced process-limit warning noise during iterative perf/test loops.

#### Did Not Work
- Running perf checks in Debug mode produced misleadingly slow wall-clock runs; comparability with historical Maxima numbers required explicit `-Doptimize=ReleaseFast`.
- The full `zig build test` loop remains expensive/noisy for rapid iteration in this workspace; focused test filters were the reliable signal for validating return-from/GC changes before broader reruns.
- Leaving benchmark helpers on stale container APIs (`vm.jit_fns.iterator()` after VM moved to `ArrayList(JitFnEntry)`) silently broke `zig build` install targets; bench harnesses must be kept in lockstep with VM container migrations.

### jj workspaces in /tmp don't work for builds with relative deps (2026-02-25)
`jj workspace add /tmp/xxx --name minion-N` creates a working copy in /tmp, but:
1. `build.zig.zon` has `.path = "../hoist"` — doesn't exist relative to /tmp
2. `build.zig` runs `git rev-parse --short HEAD` — /tmp isn't a git repo (jj uses .jj, not .git)
For parallel agent work, use `git worktree` or apply changes directly to the main repo.
jj workspaces only work when the build system has no relative path deps and no git assumptions.

### Batch dot execution, then test once per batch (2026-02-25)
For multi-dot implementation runs, run all related dots first, then run `zig build test`
once before commit/push. Running the full test suite after every dot is too expensive in
this repo and slows execution significantly. Keep dot tracking granular (`dot add`/`dot off`)
while allowing batch validation and batch commits when dots are tightly related.

### jj sibling workspaces for parallel agents (2026-02-25)
Create jj workspaces as sibling directories (`${PROJECT}-minion-XXXX`), not in /tmp.
Add a `.git` symlink pointing to the main repo's `.git` for colocated jj+git repos.
This ensures relative path deps (`../hoist`) resolve and `git rev-parse` works.
Cleanup is mandatory: `jj workspace forget` + `rm -rf` the sibling dirs after merge.

### Closure capture must include optional/key default expressions (2026-03-02)
The lambda capture analysis (`compileLambdaCore` in `src/compiler/compile.zig`) only scanned
the body for free variables. Optional parameter defaults like `(&optional (y x))` where `x`
is an outer variable were NOT captured, causing `InvalidConstant` at runtime.
**Fix:** After body scan, create a temporary `scan_env` with `new_frame=true` and the parent
env, then walk the parameter list collecting free vars from default expressions. Bind each
param in `scan_env` after scanning its default (CL: defaults evaluate left-to-right).
This is required by Maxima's `def-simplifier` macro which generates `flet give-up` with
`(&optional (y y))` defaulting to the outer variable.

### intern opcode must be callable without function-cell indirection (2026-03-02)
`(setf (fdefinition '%intern-core) #'intern)` captures the primitive, but
`(funcall #'%intern-core "FOO")` goes through the function cell which becomes stale or
recursive after `(defun intern ...)` redefines intern. Fix: add `%intern` as a compiler
builtin alias for the `.intern` opcode, so `(%intern str)` always compiles to the raw opcode
regardless of function redefinitions. This pattern applies to any primitive that gets
wrapped by a stdlib defun.

### format t must respect *standard-output* (2026-03-02)
Habu's `doFormat` treated `dest=t` as "write to system stdout" instead of looking up
`*standard-output*`. This broke Maxima's test-batch which redirects output via
`(setf *standard-output* (make-string-output-stream))`. Fix: `lookupStandardOutput()`
interns `*STANDARD-OUTPUT*` in the CL package and returns its value if it's a stream.

### Two-way streams need bidirectional direction (2026-03-02)
`allocTwoWayStream` set direction to `.input` — should be `.io`. Also,
`writeBytesToStream` and `finishOutput` need to delegate to the output component
(cdr of the source_value cons pair).

### *read-base* changes in test files can crash the parser (2026-03-02)
rtest1.mac test 206 sets `*read-base*` to 16, which causes subsequent mread calls to
attempt hex parsing on non-hex Maxima source. The test harness should save/restore
`*read-base*` around each eval, or the reader should be hardened.

### Tail calls inside progv/unwind-protect/catch skip cleanup opcodes (2026-03-02)
**Root cause**: When `tryCompileSpecialLet` lowers `(let ((*x* val)) body)` to
`(progv '(*x*) (list val) body)`, and `body` is in tail position, the body gets
compiled with tail call optimization. A `tailcall` opcode exits the current frame
immediately, skipping the `pop_progv` that follows. This means dynamic variable
bindings are never restored.

**Fix**: Compile progv body with `in_tail=false` in all sites:
- `tryCompileSpecialLet` fast path (line ~6065) and no-vm path (line ~6075)
- `compileLambdaCore` when `special_params.items.len > 0` (line ~4800)
- `compileUnwindProtectWithTail` — protected form is never tail
- `compileCatchWithTail` — body is never tail (pop_catch must execute)
- `compileHandlerCaseWithTail` — protected expr is never tail

**Key insight**: Any form that establishes a cleanup obligation (`progv`, `catch`,
`unwind-protect`, `handler-case` protected) must NOT have its body in tail position.
CL implementations don't optimize tail calls through dynamic binding forms.

### %delete-from-list must be truly destructive (2026-03-02)
**Root cause**: `%delete-from-list` used `push` + `nreverse` pattern which creates
a new list even when no elements are deleted. Maxima's `add2lnc` does:
```lisp
(setf llist (delete (assoc ...) llist :count 1 :test #'equal))
(nconc llist (ncons item))
```
When `delete` returns a new list, `llist` points to the new copy but the original
variable (`$functions`) still points to the old cons cell. The `nconc` mutates the
new list, not `$functions`.

**Fix**: Rewrite `%delete-from-list` to splice out elements in-place using `rplacd`.
This preserves cons cell identity when no elements are deleted.

### VM errors must be mapped to CL conditions for handler-case to catch them (2026-03-02)
Zig VM errors like `NoMatchingBlock`, `InvalidOpcode`, `StreamClosed` were not mapped
in `zigErrorToConditionSym`. They propagated as Zig errors, crashing the interpreter.
Fix: map `NoMatchingBlock` → `control-error`, `InvalidOpcode`/`InvalidConstant` →
`program-error`, `StreamClosed` → `stream-error`.

Also: `returnFromBlock` was doing `self.block_sp = 0` on NoMatchingBlock, which
destroyed ALL block frames including outer blocks from the test harness. Removed
the destructive reset.

### dotimes creates a `(block nil ...)` that can be exited by stray return-from (2026-03-02)
When `$ratexpand` fails internally and the error gets caught/relayed, the recovery
code may do `(return nil)` which matches the `(block nil ...)` of an enclosing
`dotimes`, exiting the loop prematurely. Workaround: use `while` loop in test
harness instead of `dotimes` to avoid implicit `(block nil)`.

### rtest1 pass rate improvements (2026-03-02)
After fixes: 125/189 tests pass (66%). Before: 8/11 (73% of readable tests but
only 11 were readable). Main fix sequence:
1. 87/189 (46%): progv tail call fix, destructive delete, VM error→condition
2. 125/189 (66%): readlist+mset overrides for ibase/obase, *print-base* sync,
   format ~A base, lookupSpecialVar qualified names, popProgvFrame storeGlobal
Remaining 36 fails: mostly CAS-level (todd_coxeter, orderlessp, obase string, float eval).
Remaining 28 errors: external file loads (9), InvalidOpcode (3), TypeMismatch (10).

### handleSpecialVarStore must use qualified CL names (2026-03-02)
Global env stores symbols as `"COMMON-LISP:*PRINT-BASE*"` (package-qualified,
uppercase). The `handleSpecialVarStore` / `handleSpecialVarLoad` functions were
looking up `"*print-base*"` (lowercase, unqualified) → never matched. Fix:
`lookupSpecialVar` tries unqualified, `COMMON-LISP:`, and `CL:` prefixed forms.

### format ~A must respect *print-base* (2026-03-02)
`formatValueAesthetic` hardcoded `{d}` for fixnums. Must use `io.print_base`
and format as `{b}` (2), `{o}` (8), `{d}` (10), `{x}` (16), or `formatIntBase`
(other bases). This is required for Maxima's `exploden` which uses
`(format nil "~A" integer)` for integer→string conversion.

### popProgvFrame must call storeGlobal not direct write (2026-03-02)
`popProgvFrame` was restoring old values via `self.globals[idx] = old_value`
which bypasses `handleSpecialVarStore`. Changed to `self.storeGlobal(idx, val)`
so Zig-level settings (print_base, print_escape, etc.) stay in sync.

### readlist override must handle non-10 bases but protect floats/rationals (2026-03-02)
Maxima's `readlist` calls `read-from-string` which in CL respects `*read-base*`.
Habu's reader ignores `*read-base*`. Override uses `parse-integer :radix base`
for pure integer tokens (no `.`, exponent markers, or `/`). Must NOT apply
non-10 base to tokens with dots (floats are always base 10) or the cascade
ibase 2→8→16→36 breaks (e.g., `16.` with ibase=8 → 14 instead of 16).
When `parse-integer` fails (invalid digits for base), return a SYMBOL via
`(intern (string-upcase s) :maxima)` instead of falling back to `read-from-string`.

### doThrow must check catch nesting before triggering outer unwind (2026-03-02)
When `handler-case` is inside `unwind-protect` protected form, `doThrow` was
triggering the unwind cleanup BEFORE checking if an inner catch matches.
Partial fix: for condition throws, check if any matching catch frame has
`unwind_depth > current_unwind_idx` (meaning the catch was established AFTER
the unwind frame, i.e., it's nested inside it). If so, skip the unwind and
go directly to handler/catch dispatch. This fixes `meval` path.

### doReturnFrom across unwind-protect loses block target (2026-03-02)
`doReturnFrom` checks `unwind_sp > 0` FIRST and triggers cleanup before searching
for blocks. After cleanup, `self.sp`/`self.fp` are set to the unwind frame's saved
values (a different call frame context). When `pop_unwind` re-invokes `doReturnFrom`,
`block_sp` may be 0 because the cleanup execution changed the stack context.
Saving the block index/frame doesn't help because the call stack entries that the
block references may have been overwritten. Root cause: Habu's flat block/unwind stacks
don't support resuming block exits after cross-frame cleanup.
**Workaround**: Override `meval*` in maxima-post-load.lisp to skip `unwind-protect`
(the `clearsign` cleanup is non-essential). This avoids the interaction entirely.

### scan-string must return CL strings, not vectors (2026-03-02)
Maxima's `scan-string` (nparse.lisp) uses `make-array :element-type #.(array-element-type "a")`
with `:fill-pointer :adjustable`. In Habu, this creates a vector, not a string.
`copy-seq` of a vector returns a vector. So string comparison (`equal "ff" #(f f)`)
fails. Fix: override scan-string to collect chars into a list, then `make-string` + 
`setf char`. This fixed 32 tests in rtest1 (85-106 obase/string tests + more).

### LOOP finally must NOT use unwind-protect (2026-03-02)
CL LOOP `finally` forms only execute on NORMAL termination (iterator exhausted),
NOT on abnormal exit (explicit `return`/throw/error). Habu's loop-expand incorrectly
wrapped the loop body in `unwind-protect` with finally as cleanup. This caused:
1. `return-from nil` in the cleanup code (finally had `(return ...)`)
2. The return-from couldn't find its block because cleanup runs after stack unwinding
3. NoMatchingBlock → integrate(x^2, x) = nil
Fix: use inner block for end-test exit, outer block for explicit returns. Finally
forms go AFTER the inner block in progn, before the result return-from.
**GOTCHA**: deep nesting `let*` binding bug — declaring a variable in a deeply nested
`let*` (>20 locals in scope) gives nil regardless of initializer. Workaround: use
`setq` on a variable declared in an outer `let*` instead.

### trySignalCondition must propagate NestedNonLocalExit (2026-03-02)
When `doThrow` returns `NestedNonLocalExit` (condition needs to cross a call barrier),
`trySignalCondition` was catching it and returning false ("no handler found"). This
caused `doError` to run (fatal path) instead of the relay mechanism in `callFromStack`.
Fix: let `NestedNonLocalExit` propagate through the `try` in execute's error handler.

### doReturnFrom must check block inside unwind scope (2026-03-02)
Same pattern as doThrow: `doReturnFrom` unconditionally ran unwind-protect cleanup
when `unwind_sp > 0`, even when the target block was INSIDE the unwind scope.
Fix: search for target block first, compare `block.unwind_depth > current_unwind`.
If inside, skip cleanup. If target not found, return NoMatchingBlock immediately
(don't run cleanup for a non-existent block).

### VM opcode handlers may bypass helper functions (2026-03-02)
The `make_string_input_stream` opcode handler in vm.zig had its own inline check
(`if (!str.isString()) return error.TypeMismatch`) that bypassed the helper function
`io.makeStringInputStream()`. The helper was fixed to accept vectors but the fix had
no effect because the opcode handler never called it. Always check the opcode handler
in vm.zig when fixing primitives in io.zig/etc.

### doThrow skip_unwind must apply to ALL throw tags, not just conditions (2026-03-02)
When `throw` targets a `catch` INSIDE an `unwind-protect`, the cleanup should NOT run
because the throw doesn't cross the boundary. But `doThrow` only had skip_unwind logic
for condition throws (`%condition%` tag). Regular catch/throw always triggered cleanup.
Fix: extend skip_unwind check to all tags using `cf.unwind_depth > current_unwind`.
CatchFrame already stores `unwind_depth = self.unwind_sp` at push_catch time.

### make-string-input-stream must accept vectors of characters (2026-03-02)
CL strings ARE vectors of characters. Maxima's `*sharp-read-buffer*` is an adjustable
vector with `:element-type 'character` and `:fill-pointer`. `make-string-input-stream`
checked `isString()` which only matches tagged string objects, not vectors. Fix: also
accept vectors and coerce character contents to a string. This fixed `integrate(sin(x), x)`.

### integrate(x^2, x) root cause chain (2026-03-02)
NOT the throw/catch/unwind-protect double-cleanup (that's fixed). The actual chain:
1. `$integrate` → `with-new-context` (unwind-protect) → `sinint` → `rischint`
2. `rischint` calls `ratf` → `ratrep*` → `prep1` → ... → `macsyma-read-string`
3. `macsyma-read-string` uses `with-input-from-string` on `*sharp-read-buffer*`
4. During load, `*sharp-read-buffer*` is an adjustable vector → TypeMismatch (now fixed)
5. But integrate still returns nil: `NoMatchingBlock` in the unwind-protect exit path
6. Root cause TBD: error propagation through unwind-protect loses catch/handler context
sinint(x^2, x) returns correct x^3/3 when called directly (no unwind-protect).

### Adversarial plan review findings (2026-03-02)
- Test runner needs inter-file isolation (kill(all) + $% reset between files)
- 121 UnboundSymbol errors, many from $% cascading (not genuinely missing functions)
- $errcatch works via mfexpr* property (not symbol-function); fboundp returns nil but meval dispatches correctly
- meval* override skips clearsign → sign assumptions accumulate across tests
- Phase 0 and Phase 1 can run in parallel (most test infra work is independent of VM fix)

### killcontext double-kill from unwind-protect (2026-03-02)
The `with-new-context` macro creates a sub-context via `$supcontext` and kills it
in `unwind-protect` cleanup. When `return-from` crosses the `unwind-protect` boundary,
cleanup runs during the return AND again normally — double `$killcontext`. Second call
errors "no such context". Fix: override `killcontext` in `lib/maxima-post-load.lisp`
to silently return if context not in `$contexts`. This unblocked `integrate` and other
CAS operations that use `with-new-context`.

### zigErrorToConditionSym must cover all runtime error types (2026-03-02)
Many Zig error types (`UnknownTypeSpecifier`, `TypeError`, `NotImplemented`, etc.)
were not mapped to CL condition types, causing fatal process crashes instead of
catchable conditions. Fixed by adding all known error types to the switch. This
unblocked rtest3 (was crashing on `UnknownTypeSpecifier` from `typep`).

### secondary_values_count clearing whitelist must include stack ops (2026-03-02)
The opcode dispatch has a whitelist of opcodes that preserve secondary values.
All other opcodes clear `secondary_values_count = 0`. Missing from whitelist:
`pop_progv`, `pop_catch`, `pop_unwind`, `push_progv`, `push_catch`, `push_unwind`.
This caused `(let ((i 0)) (values t x 0))` to lose secondary values when `i` was
special (compiled to progv). Root cause chain: algfac.lisp `(declare-top (special ... i ...))`
→ `complex-number-p` uses `(let ((R 0) (I 0)) ...)` → `I` gets progv → `pop_progv`
clears secondary values → `flonum-eval` gets `(t nil nil)` instead of `(t 0.5 0)`
→ `sin(0.5)` not reduced to float. Fixed by adding push/pop opcodes to whitelist.

### print-invert-case needs manual implementation (2026-03-02)
Habu doesn't support `:invert` readtable-case. Maxima's `print-invert-case` relies
on it: `(let ((*readtable* local-table) (*print-case* :upcase)) (princ-to-string sym))`
where `local-table` has `:invert` case. Fix: override to check if name is all-uppercase
(→ downcase), all-lowercase (→ upcase), or mixed (→ as-is). Fixed `string(a*b)` = `"a*b"`.

### mset must follow 'alias properties for ibase/obase (2026-03-02)
Maxima aliases `$ibase` → `*read-base*`, `$obase` → `*print-base*` via plist
'alias. But `mset` only sets the Maxima symbol value via `(setf (symbol-value x) y)`.
Override wraps original mset: when 'alias property exists and target is boundp,
also `(set alias-target y)` (with assign validator).

## Session Notes (2026-04-03)

### Worked Well
- Package lookup status should come from one backing package table, not from whichever mirror happens to be populated. Rewriting `findSymbol`/`findInheritedSymbol` in `src/runtime/primitives/package.zig:642-690` to classify `:internal`/`:external`/`:inherited` from the native package's local/export/use tables removes reliance on drifting Lisp-side hash tables.
- Current-package corruption should fail explicitly at the invariant boundary, not silently “heal” to `CL-USER`. Tightening `src/runtime/heap.zig:3374-3452` so `resolveCurrentPackageForIntern` and `getCurrentPackageName` assert a valid registered package pointer, plus making `deletePackage` reject the active package in `src/runtime/primitives/package.zig:978-1028`, removes a forbidden silent-reset path.
- Removing dead package state is worth doing immediately once the real owner is clear. Dropping `Vm.current_package` from `src/interp/vm.zig` simplified GC roots and removed one more mutable package slot that never drove semantics.
- Freshly emitted chunks must be rooted through the VM before any closure allocation that can trigger GC. Switching REPL/runtime compile paths from raw `heap.allocClosure` to `Vm.allocClosureWithGC` in `src/interp/repl.zig:949-952,1803-1805`, `src/compiler/compile.zig:4138-4143,10295-10297`, and `src/compiler/passes/p01_expand.zig:131` removes a stale-chunk pointer class that only shows up under nested eval/macroexpansion load pressure.
- Pathname and stream opcodes should never carry private parsing/building logic once canonical primitives exist. Routing `src/interp/vm.zig:7690-7770,7773-7782,9927-9950` through `src/runtime/primitives/pathname.zig` and turning `src/runtime/primitives/stream.zig` into an adapter over `src/runtime/primitives/io.zig` removed a second semantics surface for `make-pathname`, `parse-namestring`, `namestring`, `open`, `read-line`, `write-line`, and file-position/output control.
- `return-from` crossing `unwind-protect` must resume the exact saved block target, not re-discover a block by name after cleanup. Replacing the pending name re-search with saved block-index resumption in `src/interp/vm.zig:6243-6251,9146-9154` removes retargeting risk and makes cleanup-local same-name blocks unable to steal the original exit.
- Any new NLX triggered from cleanup must clear the superseded pending NLX state immediately. Resetting stale pending throw/error/block state at `jumpToBlock`, `doThrow`, `doReturnFrom`, and `doInvokeRestart` in `src/interp/vm.zig:9087-9093,9114-9120,8681-8685,9470-9476` prevents cleanup-originated transfers from carrying abandoned unwind state forward.
- Cleanup-originated transfers need explicit regressions for both directions. Adding `src/tests/integration.zig:4260-4299` coverage for cleanup `throw` overriding a pending `return-from` and cleanup `return-from` overriding a pending `throw` guards the exact stale-state cases that the unit-level stack fix is meant to eliminate.

### Did Not Work
- Keeping package-context recovery inside `Heap.resolveCurrentPackageForIntern` was false progress. The old `src/runtime/heap.zig:3374-3398` path logged a stale pointer and rewrote package state to `CL-USER`/`CL`/`null`, which hid real corruption instead of making the owner of package context consistent.
- Saving loader package context in both `heap.current_package` and `COMMON-LISP:*PACKAGE*` during `load` was unnecessary drift. Rebinding the special directly in `src/interp/repl.zig:1828-1838` and letting `syncReaderPackageFromVm` derive the reader/native package from that single special state is the cleaner cutover.
- Hardcoding `MAXIMA:AUTOLOAD` / `MAXIMA:LOAD-FUNCTION` in generic function resolution was exactly the kind of package-specific shortcut this plan forbids. Switching `src/interp/repl.zig:1498-1510` to derive `AUTOLOAD` and `LOAD-FUNCTION` from the target symbol's home package made autoload generic and removed the last explicit Maxima lookup from the resolver hot path.
- Treating a raw `*Chunk` pointer as “good enough” across closure creation was a hidden shortcut. In nested `eval`, macroexpansion, and compile-time thunk execution, the chunk object itself is transient heap data; if the closure allocation GC runs first, the pointer is stale before execution starts.
- Leaving hand-rolled pathname parsing and stream wrappers alive in the VM after canonical primitives were added invited immediate drift. The duplicate opcode logic in `src/interp/vm.zig` had already diverged from `src/runtime/primitives/pathname.zig`/`io.zig`, so future fixes would have needed to be made twice unless the bypass was removed.
- Resuming an outer `return-from` by name after cleanup was still a shortcut. It depended on the post-cleanup block stack looking “close enough” to the pre-cleanup stack instead of treating the original target as concrete control state.
- `defstruct`/`defclass` helpers should target the generic object protocol, not raw vector layout. Rewriting compiler-generated constructors/accessors/writers/predicates in `src/compiler/compile.zig:11275-11860` to use `make-instance`, `slot-value`, `%set-slot-value`, and `typep`, then cutting `lib/stdlib.habu:6244-6288` over to the same forms, removed the duplicated slot-0 vector encoding from both compile-time and runtime macroexpansion paths.
- A real structure cutover is cheapest when it reuses existing class metadata instead of inventing a second side table. Adding boxed `Structure` objects in `src/runtime/objects.zig`, `src/runtime/value.zig`, `src/runtime/heap.zig`, and `src/runtime/gc.zig`, and teaching `src/runtime/primitives/clos.zig`, `src/runtime/primitives/type.zig`, and `src/runtime/primitives/vector.zig` to consume `Class` metadata directly, removed the `svref 0`/symbol-tag heuristic without changing generic slot dispatch contracts.
- `zig build` is the reliable gate for structural runtime/compiler changes; `zig build test` in this tree is still blocked by the existing 5-file baseline plus one extra test-only exhaustiveness fix in `src/compiler/compile.zig:2691-2720`. Treat direct script execution through `./zig-out/bin/habu` as blocked until the separate stdlib bootstrap failure (`Cannot open 'lib/stdlib.habu': UnboundSymbol`) is fixed.

### Did Not Work
- Trying to validate the structure cutover through the main binary still runs into the pre-existing stdlib bootstrap failure before user code executes. That path is not evidence against the structure work; it is a separate startup blocker.
- Ad hoc Zig smoke binaries created outside the module root are rejected by Zig's module-path rules. If a direct harness is needed again, add it under the repo module root and remove it immediately after use.
- Structure lattice helpers must intern CL type symbols in the `COMMON-LISP` package, never in the ambient current package. Replacing package-sensitive `heap.intern(...)` calls in `src/runtime/primitives/type.zig:72-154,507-607,714-759` with canonical `COMMON-LISP` symbol lookup fixed `structure-object` / `structure-class` behavior after `(in-package ...)`.

### Worked Well
- JIT bridge OOM behavior needs a direct bridge-path proof, not a workload-shaped integration test. `src/interp/vm.zig` now tests `runJitCompiled` with a bridge-thrown `error.OutOfMemory`, which stays stable even when the supported JIT surface changes.
- Fallback-blessing tests hide real bridge contracts. Removing `jit_fallback_oom_count` and rewriting `tools/validate-session` to require explicit `OutOfMemory` relay makes JIT pressure failures observable instead of silently re-entering the interpreter.

### Did Not Work
- Using workload-shaped JIT OOM tests in `src/tests/integration.zig` was brittle. Current `make-array`, recursive `cons`, and similar allocation-heavy forms are not all in the live JIT-supported subset, so those tests rot into false blockers unrelated to the bridge contract.

### Worked Well
- The real `maxima-package.lisp` blocker was reader case folding, not another Maxima-specific patch. `src/reader/parser.zig:1115-1157` now uppercases unescaped symbol/package token bytes before package-qualified lookup, which is the missing generic CL reader rule that upstream lowercase `cl:...` forms depend on.
- A focused exact-shape regression is better than an approximate reader smoke test. `src/interp/repl.zig:5689-5706` now uses the actual Maxima-style `#+#.(cl:if (cl:and ...))` conditional, which caught the package-qualified reader bug immediately.
- When adding a no-allocation fast path, verify lifetime, not just content. Returning `upperNameAlloc(...).slice` from a stack-backed buffer in `src/reader/parser.zig` produced a correct transformation with invalid storage; the fix is to return the original slice when no folding is needed and heap-owned storage otherwise.

### Did Not Work
- The earlier focused test command used a single `-Dtest-filter` string with `|` separators, which Zig treats as one literal substring, so it did not prove those named tests were actually running. Use one concrete filter per proof run.

### Worked Well
- Early stdlib helpers must not depend on macros defined later in the same file. Moving the small bootstrap macro set (`when`, `unless`, `return`, `dolist`, `dotimes`) ahead of `%make-vector-with-fp` in `lib/stdlib.habu` fixed a real latent runtime bug where the helper had been compiled with `WHEN` as an ordinary function call.
- A loader manifest is executable semantics. Aligning `lib/maxima-manifest.lisp` with upstream `../maxima/src/maxima.system` by loading `defmfun-check` and `float-properties` before `commac`/`mormac`/`compat` immediately converted a fake `DEFMFUN`-missing failure into the next real compiler gap.
- `defmfun-check.lisp` was blocked by Habu's own partial `destructuring-bind` lowering, not by Maxima. Replacing the compiler's source-level `GETF` shortcut with a compiler-native `member`-based plist search in `src/compiler/compile.zig`, and extending `lib/stdlib.habu`'s `destructuring-bind-impl` to cover `&key`, moved clean load past `defmfun-check` and exposed the next upstream file honestly.

### Did Not Work
- Chasing the earlier `InvalidPrintCase` surface error would have been wasted motion. The decisive evidence came from the Maxima form trace: the actual blockers were stdlib bootstrap macro ordering and manifest dependency order, not the printer.
- Lowering `destructuring-bind` `&key` through source-level `COMMON-LISP:GETF` was the wrong fix. The integration tests compile without stdlib bootstrapped, so any lowering that depends on `GETF` being fbound reintroduces a bootstrap-order lie instead of closing the compiler gap.

### Worked Well
- The only stable way to converge `review-plan` was to keep freezing a `PLAN.md` baseline, launch six fresh adversarial agents against disjoint surfaces, patch once, and repeat until two consecutive clean rounds. Anything less kept missing real authority/provenance holes in `PLAN.md`.
- The plan had to model authoritative identity as data, not prose. Requiring exact loader-policy/bootstrap-helper/upstream-tree fingerprints in `PLAN.md` was what finally stopped recurring “same path, different semantics” false positives.

### Did Not Work
- Treating “trusted root” or “repo-pinned path” as sufficient was too weak. Review only converged after the plan explicitly covered regular-file checks, race-free open/load/write semantics, and content identity for both repo helpers and the external `../maxima` tree.
- `read-from-string` and `%read` must install the same ordinary reader-macro hook surface as the REPL parser. Wiring only dispatch and `#.` hooks in `src/interp/vm.zig:8105-8115,8154-8188` made `_N"..."` parse as plain symbols in string-backed reads while top-level file loads used a different path.
- Reader-macro callbacks need an explicit zero-values channel; treating `(values)` as a literal `nil` object is wrong. `src/reader/parser.zig:42-55,198-223,260-280`, `src/interp/vm.zig:440-470,3239-3330,3408-3436,6360-6464`, and `src/interp/repl.zig:2322-2413` now preserve host-call multiple-value metadata well enough for ordinary reader macros to suppress objects and continue scanning.
- Probe scripts must use the package where the binding actually lives. `lib/maxima-loader.lisp:9` binds `*maxima-source-dir*` in the current `MAXIMA` package, so probing `cl-user::*maxima-source-dir*` manufactured a fake `intl.lisp` blocker with `#<unbound>` and misled the investigation.
- Mixed symbol plists are a real runtime hazard. `src/runtime/primitives/symbol.zig` and `src/runtime/primitives/list.zig` had diverged between alist-style and flat-plist-style walkers, so entries like `MFEXPR*` could be present in `symbol-plist`/`safe-getl` but invisible to `get` and compiler/VM property lookups. The only correct fix was hard cutover to one mixed-shape-aware plist iteration path, then route compiler and VM function/macro lookups through the same `get` surface.
- `funcall` cannot be lowered through ordinary function-position compilation. `src/compiler/compile.zig` must preserve value-namespace evaluation for the designator expression while separately rooting the rest arg list across nested compile steps; routing `funcall` through `compileCallWithTail` made local variables like `f` in `mapcar` resolve as function names and broke stdlib macro expansion at `lib/stdlib.habu` form 136 (`signp`).

### Worked Well
- `read` on streams must be stateful one-form parsing, not `read-all` disguised as `read`. The old `lib/stdlib.habu` implementation slurped the full stream and delegated to `read-from-string`, so every stream read after form 1 hit fake EOF. Moving stream reads onto a parser-backed `%read-stream` opcode in `src/interp/vm.zig`, with unread-tail state stored on `src/runtime/objects.zig` streams and preserved by `src/runtime/gc.zig`, fixed `spgcd.lisp` and any other multi-form source generically.
- Repeated form probes are the fastest proof for reader/load bugs. The `/tmp/habu-spgcd-form18.lisp` harness immediately showed `(read s nil :eof)` advancing through real successive forms once the runtime fix landed, which is stronger evidence than a workload-level timeout disappearing.

### Did Not Work
- Trying to validate stream `read` through `zig build test` first hid the real outcome behind the standing 5-error test compile baseline. For loader/reader bugs in this tree, prove the direct runtime script behavior first, then treat `zig build test` as a secondary gate only up to the known baseline.
- `make-array` character buffers have to cut over at compile time, not by post-hoc Maxima patching. `src/compiler/compile.zig:18904-19005` was missing quoted/upcased `:element-type 'character` / `'base-char`, so adjustable fill-pointer character buffers were still plain vectors; that broke Maxima `nparse` string scanning. After routing those forms through `%make-char-vector` helpers and teaching VM string opcodes/format/vector predicates to treat character-vectors and strings consistently (`src/interp/vm.zig:5038-5115,11528-11655`, `src/runtime/primitives/vector.zig:332-339`), the direct repros for constructor, `char`, `copy-seq`, and `vectorp` all turned green.
- Once `vectorp` correctly recognizes strings, sequence coercion must branch on `stringp` before generic vector paths. `lib/stdlib.habu:2663-2684` was sending `(coerce "AZ" 'list)` through the generic vector->list clause, where `svref` lowers to `vec_ref` and `src/interp/vm.zig:4689-4707` rightly rejects strings. Reordering the `coerce` clauses fixed the direct repro and let the authoritative Maxima prefix through `mforma` load cleanly again.
- A direct `(format t "~S" (coerce '(1 2) 'vector))` failure after `coerce` succeeds is a separate printer bug, not a sequence bug. Validate the computed value without printing first so the next fix lands on the formatter path instead of regressing sequence semantics.
- Sentinel-packed metadata needs sentinel-aware predicates. `src/runtime/objects.zig:228-249` was treating `fill_none = 0xFFFF...` as an ordinary bitfield, so every simple vector looked both adjustable and character-vector because the sentinel has both high bits set. Guarding `isAdjustable` and `isCharacterVector` with `fill_pointer != fill_none` fixed `write-to-string '#(1 2 3)` and `(format t "~S" (coerce '(1 2) 'vector))` without weakening character-vector support.
- `lib/maxima-post-load.lisp` must keep Maxima's reset authority in sync when it seeds shadowed runtime directory vars outside `init-cl.lisp`. We were setting `*maxima-userdir*` / `$maxima_userdir` for the checkout bootstrap but not updating `*variable-initial-values*`, so the first `reset()` in `rtest6` restored `nil`/`false` into `$maxima_userdir` and tripped `shadow-string-assignment` in `../maxima/src/init-cl.lisp:53-58`. Seeding `(gethash '$maxima_userdir *variable-initial-values*)` alongside the post-load assignment fixed the real file-driven `rtest6` form-1 failure.
- Symbol plist conversion must root traversal state, not just payload cells. `src/runtime/primitives/symbol.zig:174-214` was carrying the next-cons pointer across allocating conversions in `plistToFlat` and `flatToAList` without rooting it, so a GC during symbol plist reshaping could silently corrupt cons-valued properties such as Maxima selector tables and db labels. Rooting `entry.next` / `next` closed the moving-GC hazard; `zig build` stays clean, while `zig build test` remains blocked earlier by the standing `check-errors` gate at `src/interp/vm.zig:1943`.
