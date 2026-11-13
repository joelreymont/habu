---
title: "TRUSTED: dies; PRIM axioms remain for the foreign handful"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T09:53:28.150430+02:00"
---

Goal: genuine engine, syscall and FFI boundaries have package-owned primitive
effects; ordinary Forth is checked; the TRUSTED: definer is removed.

The native-build children and B8 recording/authority design are completed.
Do not reopen their old shim census, staged B8/B9 schedule or temporary probe
artifacts. Remaining work is owned by e08e345f (package-owned primitive
boundaries), 41e973ce (core/compiler bodies), f872acb0 (library/tools bodies),
8d8cdc19 (typed test source evaluation), and 42b30edd (final definer/flag removal).
Specific remaining casts are handled with their actual caller in those lanes,
rather than reopening the completed ad2e237d cast sweep.

The user retained this project after incremental builds and test-suite work.
When resumed, inspect each current body through its real product or whitebox
load path before changing it. Use its actual effect and authority refusal to
distinguish checked code, an explicit representation cast and a necessary
private foreign boundary. Do not infer a boundary from the old census or the
number of tokens in a body. Source-window behavior must be checked on a
matching engine; a booted host alone does not qualify a from-source path.

The final removal needs the repository native suite and engine convergence;
recovery qualification applies to changed seed/mirror paths. Close this parent
only when 42b30edd is implemented and qualified.

Acceptance case 2026-11-10 (bob; merged from habu-shrink-trusted-to-f4e84345 "Shrink TRUSTED: to real boundaries", one cause with this dot): Joel 2026-10-31: shrink TRUSTED: after the T0 release, not before. Today: 102 TRUSTED: definitions in src (27 files; src/lib/xml/c2.f 31, checker.f 8, verify-source.f 7, include.f 7), 16 in tools, 69 in test. docs/forth.md says never add TRUSTED: to bypass a missing checker model and keeps it for bodies the checker cannot model (evaluate, metaprogramming) behind a named, tested boundary; many uses are plain variable reads or xt fetches. Census every use; give each the checked form or the checker the model it lacks at its owner; keep only the uncheckable ones. t0.f TYPE-ENTRY/XT-STORE-ENTRY/DOES-PATCH-ENTRY duplicate elaborate.f's: one shared copy or none.

Batch 1 landed (habu-shrink-trusted-to-f4e84345, eight commits ending "Row the checker words the owner calls"): 75 TRUSTED: definitions became checked : or CAST: definitions; TRUSTED: definitions in src 102 -> 55, tools 16 -> 5, test 69 -> 52. The tools sites were the 11 REC-* writers in tools/native-unit-object.f.

The xt feature landed (habu-let-signatures-name-5931014b, habu-check-the-primitive-4b61908e): a signature names a ticked primitive's type as xt, so the tick-only entry words became checked. TRUSTED: definitions in src 55 -> 47 (elaborate.f 4 -> 0, t0.f 3 -> 0, select-x64.f 1 -> 0, trap.f 1 -> 0, dict.f 0 -> 1 for the one shared DOES-PATCH-ENTRY); tools 5, test 52 unchanged.

The checker verdict words landed (habu-publish-json-diags-116b6142): REPORT-UNCHECKABLE, MULTI-ERR-BEGIN and MULTI-ERR-END moved into checker.f: the TRUSTED: CHECK-BODY copy is deleted and MULTI-ERR-BEGIN is checked. TRUSTED: definitions in src 47 -> 45 (verify-source.f 7 -> 6, cell-effects.f 4 -> 3).

The C2 disposer landed (habu-check-the-c2-99fd69f8, on habu-pass-a-fresh-bb992e31): ALLOC-DISPOSE is checked through a C2-MEM callback pack. TRUSTED: definitions in src 45 -> 44 (c2-owner.f 1 -> 0); tools 5, test 52 unchanged.

Repair advice: E-UNSAFE's repair class is now `remove_unsafe_word` (src/core/render.f REPAIR-CLASS); E-CAP-TRUSTED keeps `trusted_boundary_required`.

Census on master 7160e349 (/tmp/jerry-big/trusted-census-tc{1,2,3}.md): 99 TRUSTED: definitions, src 44, tools 3, test 52. The three in tools (wasm-build.f and build-fixpoint-certify.f REWIND-CHECK-OFF, tier0-profile.f ABLATE) turn checking off with set-check: boundaries. Every test/ use the census found convertible is converted; the one checker gap left is test/stripped-quotation-subject.f:29 PRIMITIVE-XT, which as `:` still fails "at '[']' expected: [ n -- n ] actual: xt" (a tick of a primitive with several rows). In src every use is a boundary except src/lib/xml/c2.f's 8, which wait on carl's lane cown (XML:EMPTY-CURSOR), and two the lead ruled boundaries: definers.f:623 DEF-PREFIX-BIND calls CHECKER-PREFIX:BIND?, whose primitive row the checker marks trusted-only on purpose, and native-runtime.f:217 BIND-OWNER calls DECLARATIONS, which returns the checker's private declaration record. Each binds engine cells to checker-private state; giving the callees checked rows would hand that state to every checked caller. Once c2.f's 8 go, the shrink is at its floor. Not designed: what replaces the checker's trusted-only primitive rows (PRIM-TRUSTED-ONLY!, 13 marks at checker.f 11458-11798 plus the rows PRIM-SPEC flags at 11371) when TRUSTED: retires, since those marks are the trust boundary itself.

The shrink is at its floor. cown landed (792a7a10), and src/lib/xml/c2.f's 8 were each measured (habu-check-the-xml-a8c68880): every checked form is refused (E-CAST-LINEAR, E-LINEAR-OWNER, E-CAST-SCOPE, exclusive copy, E-CAP-TRUSTED), and lifting the refusal needs a public word that would let any package forge an XML reader or alias an exclusive view. They stay as boundaries with DEF-PREFIX-BIND and BIND-OWNER. habu-check-a-cast-86464052 closed as dropped by the cast ruling. What is left here: habu-keep-renderer-working-c5d7b8e6, test/stripped-quotation-subject.f:29 PRIMITIVE-XT, and the trusted-only primitive rows' replacement when TRUSTED: retires.

Acceptance case 2026-11-10 (bob; merged from habu-sweep-trusted-out-41e973ce "Sweep TRUSTED: out of the core and compiler", one cause with this dot): Retire remaining unchecked bodies under src/core, src/compiler, src/habu and
src/os. Ordinary Habu becomes checked code; genuinely unmodeled engine,
syscall and FFI operations stay small explicit primitive boundaries owned by
their package. Do not substitute an axiom for an ordinary checked word.

The old B7/B8 row-recording and authority mechanism is implemented, including
the declared-row window path and the declaration shim removal. Its historical
source counts and schedule are not current prerequisites. The stricter
package boundary/final flag work remains with e08e345f and 42b30edd.

When resumed after builds and test-suite work, inspect the current definitions
and their real load paths. Known remaining candidates include generated
declaration raw-memory/runner bodies, structure-make's row emitter and
verify-source's checker dispatch. Probe before replacing each one: checked
bodies use their actual bound effects; representation changes use explicit
casts; source-window/owner access must work at the responsible boundary.
Keep multi-call or raw-memory bodies separate from simple forwarding shims.

Use one coherent change per owner/file family, preserving existing positive
and rejection behavior. Before adding tests, inspect the current downstream
and source-window E2Es. Acceptance is no TRUSTED: definitions in the affected
src surface, a matching engine, the required full native suite and convergence
for shared checker/compiler/capture changes. Recovery checks apply when a
seed, mirror or recovery dependency changes. Do not repeat completed row
recording, resurrect obsolete temporary probes or weaken required boundaries.

Acceptance case 2026-11-10 (bob; merged from habu-sweep-trusted-out-f872acb0 "Sweep TRUSTED: out of lib and tools", one cause with this dot): Problem: 190 TRUSTED: definition sites remain in src/lib/ (112) and tools/ (78, effect-store-census.f 25). Joel (2026-09-16): TRUSTED: is to be retired entirely. The FFI consumers in src/lib/ (udp4, tcp4, serial, zip-ffi, f64-text, task, fs-identity, process-pty-handle) are swept by aspen through a checked FFI:CALL inside package FFI once habu-pkg-owned-prim-e08e345f lands; this dot owns every other src/lib/ and tools/ site. Acceptance: `rg -c "^\\s*TRUSTED:" lib tools` counts only the files aspen owns until those land, then 0; each converted body is checked, with a package-owned PRIM axiom only where a primitive is genuinely unmodelled; suites of every touched file green; full gate green. Files: src/lib/*.f (except the FFI consumers listed), tools/*.f. Verify: rg count; per-file suites; bin/hb --load test/run.f. Depends: none for a body that calls no trust-boundary primitive (the majority; start there). A body that calls a PE-TRUSTED-ONLY primitive (checker.f 6925-6945 code-injection prims, ffi-call-bounded) waits for the mechanism decision recorded in habu-delete-the-trusted-42b30edd: hazel recommends dropping the bit and making those primitives private to their owning packages, so package privacy, not a checker flag, bounds who may call them; the FFI consumers go through aspen FFI declarer. Ownership: src/lib/ tools/ minus the FFI consumers. Parent: habu-trusted-dies-prim-4fd12d60.

## Leaves (2026-10-01 plan; census in the parent)

lib, 99 non-evaluate sites measured through `bin/hb --load`: (d) 64 (ffi-call-* 49 for the FFI declarer lane; patch32 in FFI test helpers 12; task-entry task.f:1038, task-test.f:1444; executable-build.f:7); (b) 25 at the plan, 6 at d273e641 after the LINEAR: batch (master c33ae867) converted the mint and erase sites in byte-edit.f, json-read.f, process-pty-handle.f, xml/source.f and xml/state.f: byte-edit.f:29 ADDRESS, xml/scalar.f:33 BYTE-ADDRESS, task.f:189,191,196 (TASK-N>PTR, TASK-CELL>PTR-SLOT, TASK-CELL>XT-SLOT), aio.f:310 AIO-MAPPED>PTR; (e) 2 (genio.f:167 ENGINE-ROW, fs-mutate.f:600 FS-MUT-ARM-EXIT); (a) 5; unmeasured 3 (memory.f, aio-macos.f: baked, duplicate on reload; aio.f second site).
tools, 97 non-evaluate: (d) 13 (set-tier x7, set-check sites, native-unit-compile.f:117, native-unit-object.f:256-258); (b) 18 (xt/pointer casts in native-build-core.f, native-unit-build-core.f, native-source-view.f, native-unit-capture.f, aot-chain-capture.f:215, imgdump.f:78, imagedisasm.f:46, check-core.f); (e) 8 (DISPATCH-CELL in chain-scale.f:143, compile-floor.f:127, compile-scaling.f:185; native-unit-compile.f:33-35; data-residue-census.f:35; decl-gen-probe.f:56); (c) 41 (effect-store-census.f shims onto EFF-REC/EN-*/ER-*; decl-gen-probe.f onto TFAM-*/TDECL-*/SUMV-*), retired with B8; (a) 15.

B1 (worker-light, ready): lib (a) sites (engine-id.f, serial.f 1, task.f 2 multi-token, src/lib/test/subject.f non-evaluate) become `:`; scalar casts become `CAST:` inside the owning package with a one-line reason: process-pty-handle.f:54-59 (six), byte-edit.f:28 ADDRESS, xml/scalar.f:33. Verify: `bin/hb --load src/lib/<file>` green, each file's suites (test/gate-stdlib-cases.f names them), per-file rg count equals the residual (d)/(b-pointer) count.
B2 (worker, ready): tools (a) sites (two-generation-probe.f 8, callable-arity-probe.f, row-shape-probe.f, native-build-core.f 2, native-unit-compile.f 3) become `:`; (e) raw cells declared at their owner (`PTR-VARIABLE` or a declared accessor, docs/forth.md "Raw storage never holds an address"): DISPATCH-CELL x3, native-unit-compile.f:33-35, data-residue-census.f:35. Verify: each tool loads (usage exit is fine), the SUITEs that run them, rg counts.
B3 (worker, after B11): mint the set-tier owner; the seven tools SELECT-TIER/SET shims and the test SELECT shims (prim-owner-scope-child.f:34, primitive-trust-child.f:24, does-empty-clause.f:44, compiler/compile-floor.f:131, native-match.f:647) call its checked public word. Verify: each tool loads; those suites; engine fixpoint.
B4 (worker-light, after 12ffa000 and src/lib/test/eval.f): the 11 tools evaluate sites (decl-gen-probe.f:55 is the eleventh) per the evaluate design; `rg -n 'TRUSTED: .*evaluate' tools` empty.
Decided: patch32 test helpers follow the (d) recipe: they call the publisher package's checked public word, no fixture package owns patch32. The 49 ffi-call-* sites go through the checked FFI declarer (habu-pkg-owned-prim-e08e345f lane).

B1 landed 2026-10-01: five (a) sites are checked `:` definitions (engine-id.f ENGINE-SELF-LINUX, serial.f SET-CALL, task.f TASK and FACILITY, test/subject.f STACK-ARM); lib count in the touched files 35 -> 30. The nine sites listed as scalar casts are not scalar: byte-edit.f:28 ADDRESS and xml/scalar.f:33 BYTE-ADDRESS take a pointer operand (E-CAST-CLASS) and wait for B10a in habu-turn-deliberate-cast-ad2e237d; process-pty-handle.f:54-59 are linear token mint and erase (E-CAST-LINEAR) and belong to habu-mint-and-erase-72e83e7a. Dead dot slugs remain in comments at src/lib/task.f:174, src/lib/test/subject.f:56 and test/compiler/native-wide-mem.f:90; the lane that converts each site drops its line.

B2 landed 2026-10-02 (xlrlnxvy e503de54, Retire certifying tools TRUSTED: sites): (a) sites are `:`, (e) raw cells declared at their owner; tools/** TRUSTED: 114 -> 101; kept sites listed by owner in the commit description. Fable re-review accepted all five items.

B4 landed 2026-10-02 (yktltxkw 857d206c, Retire TRUSTED evaluate wrappers under tools): the 12 tools sites that reached evaluate (BUILD in native-build.f, native-unit-build.f, native-bootstrap.f; app-build.f STARTUP; BF-EVAL-N; four EVAL$; JIT-EVALUATE; decl-gen-probe EV and TRY) run closed texts; tools/** TRUSTED: 110 -> 98, `rg -n 'TRUSTED: .*evaluate' tools` empty. Public for closed texts: ORIGIN/UNKNOWN-ORIGIN words, BF-MARK!, JIT-EVALUATE. Untested: native-bootstrap.f and decl-gen-probe.f die at load identically on the parent (-8286 REGISTER, E-UNDEFINED TFAM-ACTIVE-PKG$). Fable review accepted all four items.

Census gap, for the lane that retires the effect-store-census.f (c) shims with B8: the census WALK and SHAPE know no EN-FORALL or EN-BVAR (the engine sweep marks both since louzyyqz), so a quantified effect's children go uncounted and unmarked. That lane adds T-FORALL/T-BVAR beside T-ROW as checked words (no new TRUSTED:), a WALK branch recursing N-A, N-B and N-C for FORALL with BVAR a leaf, SHAPE folding N-C and N-D for FORALL, and one kept forall-taking word in test/effect-store-census-test.f window 4 that certifies after CT-SWEEP with BALANCED holding, red before the branch.

LINEAR: batch landed (habu-mint-and-erase-72e83e7a, six commits ending "Mint the JSON reader with LINEAR:"): the (b) linear mint/erase trios in byte-edit.f, json-read.f, process-pty-handle.f, xml/source.f and xml/state.f are checked `LINEAR:` rows in their owning packages; TRUSTED: 1216 -> 1197. byte-edit.f:29 ADDRESS stays a pointer cast waiting on B10a.

Batch 1 landed (habu-shrink-trusted-to-f4e84345, eight commits ending "Row the checker words the owner calls"): 75 TRUSTED: definitions became checked : or CAST: definitions; TRUSTED: definitions in src 102 -> 55, tools 16 -> 5, test 69 -> 52. The tools sites were the 11 REC-* writers in tools/native-unit-object.f.

The xt feature landed (habu-let-signatures-name-5931014b, habu-check-the-primitive-4b61908e): TRUSTED: definitions in src 55 -> 47, tools 5, test 52.
